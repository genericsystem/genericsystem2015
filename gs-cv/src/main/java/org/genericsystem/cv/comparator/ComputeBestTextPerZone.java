package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
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
import org.genericsystem.cv.model.ZoneText.ZoneTimestamp;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ComputeBestTextPerZone {

	private final static String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";
	private static Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] mainArgs) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, ZoneTimestamp.class, Score.class, MeanLevenshtein.class);
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
		try {
			engine.getCurrentCache();
		} catch (IllegalStateException e) {
			logger.error("Current cache could not be loaded. Starting a new one...");
			engine.newCache().start();
		}
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);

		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue())).toList();
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");
		ImgFilterInstance bestInstance = imgFilter.setImgFilter("best");

		logger.debug("Processing doc: {}", docInstance.getValue());

		for (ZoneInstance zoneInstance : zoneInstances) {
			logger.debug("Zone n°{}", zoneInstance.getValue());

			ZoneTextInstance realTextInstance = zoneText.getZoneText(docInstance, zoneInstance, realityInstance);

			// If not supervised, compute the best text
			// TODO: deal with the case where a field must be left blank
			if (realTextInstance == null || realTextInstance.getValue().toString().isEmpty()) {
				// Map containing the distinct OCR texts as a key, and the
				// names of the imgFilters that gave this OCR
				Map<String, List<String>> ocrResults = new ConcurrentHashMap<>();

				for (ImgFilterInstance imgFilterInstance : imgFilterInstances) {
					ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
					if (zti == null) {
						// TODO case where zti doesn't exist == filter has
						// not been applied
						logger.error("No text found for {} => zone n°{}, {}", docInstance.getValue(), zoneInstance.getValue(), imgFilterInstance.getValue());
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

				// Map containing each distinct OCR text as key, and the
				// corresponding ponderation as a value (i.e., the sum of
				// the individual scores of each filter that gave this
				// string)
				Map<String, Float> ocrElection = new ConcurrentHashMap<>();

				ocrResults.entrySet().forEach(entry -> {
					Float ocrWeight = 0f;
					for (String filter : entry.getValue()) {
						ScoreInstance scoreInstance = score.getScore(zoneInstance, imgFilter.getImgFilter(filter));
						if (scoreInstance == null) {
							logger.error("No score found for zone n°{} and filter {}", zoneInstance.getValue(), filter);
						} else {
							ocrWeight += (Float) scoreInstance.getValue();
						}
					}
					ocrElection.put(entry.getKey(), ocrWeight);
				});

				String bestText = ocrElection.entrySet().stream().max(Map.Entry.comparingByValue()).get().getKey();
				ZoneTextInstance zti = zoneText.setZoneText(bestText, docInstance, zoneInstance, bestInstance);
				zti.setZoneTimestamp(ModelTools.getCurrentDate()); // TODO: concatenate with previous line?

			} else {
				// If supervised, set the supervised text to best
				ZoneTextInstance zti = zoneText.setZoneText(realTextInstance.getValue().toString(), docInstance, zoneInstance, bestInstance);
				zti.setZoneTimestamp(ModelTools.getCurrentDate()); // TODO: concatenate with previous line?
			}

			engine.getCurrentCache().flush();
			// System.out.println("Best text : " +
			// zoneText.getZoneText(docInstance, zoneInstance, bestInstance));
		}
	}
}
