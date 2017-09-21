package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.docPattern.OCRPlasty;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ModelTools;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComputeBestTextPerZone {

	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] mainArgs) {
		final Root engine = FillModelWithData.getEngine(gsPath);
		engine.newCache().start();
		compute(engine);
		engine.close();
	}

	public static void compute(Root engine) {
		final String docType = "id-fr-front";
		compute(engine, docType);
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void compute(Root engine, String docType) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();
		for (DocInstance docInstance : docInstances) {
			computeOneFile(engine, docInstance, docType);
		}
	}

	public static void computeOneFile(String filename, String docType) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);
		engine.newCache().start();
		computeOneFile(engine, filename, docType);
		engine.close();
	}

	public static void computeOneFile(String filename) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);
		final String docType = "id-fr-front";
		engine.newCache().start();
		computeOneFile(engine, filename, docType);
		engine.close();
	}

	public static void computeOneFile(Root engine, String filename, String docType) {
		Doc doc = engine.find(Doc.class);
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		DocInstance docInstance = doc.getDoc(filename, (DocClassInstance) currentDocClass);
		computeOneFile(engine, docInstance, docType);
	}

	// TODO: pass docClassInstance as a parameter instead of docType?
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void computeOneFile(Root engine, DocInstance docInstance, String docType) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);

		Snapshot<ZoneInstance> zoneInstances = (Snapshot) currentDocClass.getHolders(engine.find(ZoneGeneric.class));
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");
		ImgFilterInstance bestInstance = imgFilter.setImgFilter("best");

		logger.debug("Processing doc: {}", docInstance.getValue());
		zoneInstances.forEach(zoneInstance -> {
			logger.debug("Zone n°{}", zoneInstance.getValue());
			ZoneTextInstance realTextInstance = zoneText.getZoneText(docInstance, zoneInstance, realityInstance);
			if (realTextInstance == null || realTextInstance.getValue().toString().isEmpty()) {
				// If not supervised, compute the best text
				String bestText = computeBestTextOcrPlasty(engine, docInstance, docType, zoneInstance);
				if (null != bestText) {
					zoneText.setZoneText(bestText, docInstance, zoneInstance, bestInstance).setZoneTimestamp(ModelTools.getCurrentDate());
				} else {
					logger.debug("No OCR data found for {}", docInstance.getValue());
				}
			} else {
				// If supervised, set the supervised text to best
				zoneText.setZoneText(realTextInstance.getValue().toString(), docInstance, zoneInstance, bestInstance).setZoneTimestamp(ModelTools.getCurrentDate());
			}
			// engine.getCurrentCache().flush(); // FIXME: timeout while flushing
		});

	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static String computeBestTextOcrPlasty(Root engine, DocInstance docInstance, String docType, ZoneInstance zoneInstance) {
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Snapshot<ImgFilterInstance> imgFilterInstances = (Snapshot) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue()));

		List<String> ocrTexts = new ArrayList<>();
		imgFilterInstances.forEach(imgFilterInstance -> {
			ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
			if (zti == null) {
				logger.debug("No text found for {} => zone n°{}, {}", docInstance.getValue(), zoneInstance.getValue(), imgFilterInstance.getValue());
			} else {
				String text = zti.getValue().toString();
				ocrTexts.add(text);
			}
		});
		String bestText = OCRPlasty.ocrPlasty(ocrTexts);
		logger.debug("Best text: {}", bestText);
		return bestText;
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static String computeBestText(Root engine, DocInstance docInstance, String docType, ZoneInstance zoneInstance) {
		// TODO: deal with the case where a field must be left blank
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);
		Snapshot<ImgFilterInstance> imgFilterInstances = (Snapshot) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue()));

		// Map containing the distinct OCR texts as a key, and the names of the imgFilters that gave this OCR
		Map<String, List<String>> ocrResults = new ConcurrentHashMap<>();
		for (ImgFilterInstance imgFilterInstance : imgFilterInstances) {
			ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
			if (zti == null) {
				// TODO case where zti doesn't exist == filter has not been applied
				logger.debug("No text found for {} => zone n°{}, {}", docInstance.getValue(), zoneInstance.getValue(), imgFilterInstance.getValue());
			} else {
				String text = zti.getValue().toString();
				List<String> filters = ocrResults.get(text);
				if (filters == null) {
					filters = new ArrayList<>();
				}
				filters.add(imgFilterInstance.getValue().toString());
				ocrResults.put(text, filters);
			}
		}

		// Map containing each distinct OCR text as key, and the corresponding ponderation as a value (i.e., the sum of the individual scores of each filter that gave this string)
		Map<String, Float> ocrElection = new ConcurrentHashMap<>();
		ocrResults.entrySet().forEach(entry -> {
			Float ocrWeight = 0f;
			for (String filter : entry.getValue()) {
				ScoreInstance scoreInstance = score.getScore(zoneInstance, imgFilter.getImgFilter(filter));
				if (scoreInstance == null) {
					logger.debug("No score found for zone n°{} and filter {}", zoneInstance.getValue(), filter);
				} else {
					ocrWeight += (Float) scoreInstance.getValue();
				}
			}
			ocrElection.put(entry.getKey(), ocrWeight);
		});

		if (null != ocrElection && !ocrElection.isEmpty()) {
			return ocrElection.entrySet().stream().max(Map.Entry.comparingByValue()).get().getKey();
		} else {
			return null;
		}
	}
}
