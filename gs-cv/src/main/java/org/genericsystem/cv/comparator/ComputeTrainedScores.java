package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reinforcer.tools.Levenshtein;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ComputeTrainedScores class computes the {@link Score} and the {@link MeanLevenshtein} for each zone and each filter. The data is retrieved from GS, and stored in GS.
 * 
 * @author Pierrik Lassalas
 */
public class ComputeTrainedScores {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";

	/**
	 * Used for a "strict" string comparison. The {@link Levenshtein} distance is zero only if two strings are identical.
	 */
	public static final Boolean BE_STRICT = Boolean.TRUE;
	/**
	 * Allow some variations during the string comparison. For example, spaces and periods are removed before the {@link Levenshtein} distance is computed.
	 */
	public static final Boolean BE_GENTLE = Boolean.FALSE;

	public static void main(String[] mainArgs) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);
		engine.newCache().start();
		final String docType = "id-fr-front";
		final boolean useStrict = false;
		compute(engine, docType, useStrict);
		// engine.getCurrentCache().flush();
		engine.close();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void compute(Root engine, String docType, Boolean useStrict) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein meanLevenshtein = engine.find(MeanLevenshtein.class);

		logger.info("Current doc class : {} ", currentDocClass);

		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue())).toList();
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");

		for (ZoneInstance zoneInstance : zoneInstances) {
			logger.debug("=> Zone {}", zoneInstance);
			for (ImgFilterInstance imgFilterInstance : imgFilterInstances) {
				int lev = 0; // contains the sum of all Levenshtein distances for a given zone
				int count = 0; // contains the number of "perfect" matches
				int totalDocs = docInstances.size(); // contains the number of documents

				// Loop over all documents in this class
				for (DocInstance docInstance : docInstances) {
					ZoneTextInstance realZti = zoneText.getZoneText(docInstance, zoneInstance, realityInstance);
					// Do not attempt the computation if the document was not supervised
					if (realZti == null) {
						logger.debug("Document {} on zone {} was not supervised (passed)", docInstance.getValue(), zoneInstance.getValue());
						// Decrement the total size, since this value will not be accounted for in the statistics
						totalDocs--;
					} else {
						String realText = (String) realZti.getValue();
						ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
						// Do not proceed if the zoneText does not exists (i.e., the algorithm was not applied to this image)
						if (zti == null) {
							logger.debug("No text found for {} => zone n°{}, {}", docInstance.getValue(), zoneInstance.getValue(), imgFilterInstance.getValue());
							// Decrement the total size, since this value will not be accounted for in the statistics
							totalDocs--;
						} else {
							String text = (String) zti.getValue();
							int dist;
							if (useStrict.equals(BE_STRICT))
								dist = Levenshtein.distance(text.trim(), realText.trim());
							else
								dist = Levenshtein.distance(text.replaceAll("[\n ,.]", "").trim(), realText.replaceAll("[\n ,.]", "").trim());
							count += (dist == 0) ? 1 : 0;
							lev += dist;
						}
					}
				}
				if (totalDocs > 0) {
					float probability = (float) count / (float) totalDocs;
					float meanDistance = (float) lev / (float) totalDocs;
					score.setScore(probability, zoneInstance, imgFilterInstance).setMeanLev(meanDistance);
					// ScoreInstance scoreInstance = score.getScore(zoneInstance, imgFilterInstance);
					// System.out.println(String.format("score: %f | ml: %f", scoreInstance.getValue(), meanLevenshtein.getMeanLev(scoreInstance).getValue()));
				} else {
					logger.error("An error has occured while processing the score computation of zone n°{} (class: {})", zoneInstance.getValue(), docType);
				}
			}
		}
	}

	// Untested yet!
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void clearStatistics(Root engine, String docType) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		Score score = engine.find(Score.class);
		logger.info("Current doc class: {} ", currentDocClass);
		Snapshot<ZoneInstance> zoneInstances = (Snapshot) currentDocClass.getHolders(engine.find(ZoneGeneric.class));
		Snapshot<ImgFilterInstance> imgFilterInstances = (Snapshot) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue()));
		zoneInstances.forEach(zone -> imgFilterInstances.forEach(ifi -> score.getScore(zone, ifi).remove()));
	}
}
