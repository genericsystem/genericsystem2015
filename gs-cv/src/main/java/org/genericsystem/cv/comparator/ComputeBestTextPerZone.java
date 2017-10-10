package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.utils.ModelTools;
import org.genericsystem.cv.utils.OCRPlasty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComputeBestTextPerZone {

	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] mainArgs) {
		final Root engine = FillModelWithData.getEngine(gsPath);
		final String docType = "id-fr-front";
		engine.newCache().start();
		compute(engine, docType);
		engine.close();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void compute(Root engine, String docType) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		Snapshot<DocInstance> docInstances = (Snapshot) currentDocClass.getHolders(engine.find(Doc.class));
		for (DocInstance docInstance : docInstances) {
			computeOneFile(engine, docInstance);
			// engine.getCurrentCache().flush();
		}
	}

	/**
	 * Computes the statistics for a given file.
	 * <p>
	 * <b>This function does not flush the cache!</b>
	 * 
	 * @param engine - the engine where the data should be saved
	 * @param docInstance - the document that will be processed
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void computeOneFile(Root engine, DocInstance docInstance) {
		Generic currentDocClass = docInstance.getDocClass();
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
				String bestText = computeBestTextOcrPlasty(engine, docInstance, zoneInstance);
				if (null != bestText) {
					zoneText.setZoneText(bestText, docInstance, zoneInstance, bestInstance).setZoneTimestamp(ModelTools.getCurrentDate());
				} else {
					logger.debug("No OCR data found for {}", docInstance.getValue());
				}
			} else {
				// If supervised, set the supervised text to best
				zoneText.setZoneText(realTextInstance.getValue().toString(), docInstance, zoneInstance, bestInstance).setZoneTimestamp(ModelTools.getCurrentDate());
			}
		});

	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static String computeBestTextOcrPlasty(Root engine, DocInstance docInstance, ZoneInstance zoneInstance) {
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
		if (null == ocrTexts || ocrTexts.isEmpty()) {
			logger.debug("No text found for {} => zone n°{}", docInstance.getValue(), zoneInstance.getValue());
			return null;
		} else {
			String bestText = OCRPlasty.correctStrings(ocrTexts, OCRPlasty.RANSAC.NONE);
			logger.debug("Best text: {}", bestText);
			return bestText;
		}
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static String computeBestText(Root engine, DocInstance docInstance, ZoneInstance zoneInstance) {
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
