package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.Levenshtein;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ComputeTrainedScores class computes the {@link Score} and the {@link MeanLevenshtein} for each zone and each filter. The data is retrieved from GS, and stored in GS.
 * 
 * @author Pierrik Lassalas
 */
public class ComputeFilterParamOptimization {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final static String docType = "id-fr-front";
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model2/";
	private final static Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);

	public static void main(String[] mainArgs) {
		engine.newCache().start();
		compute();
		printResults(10);
		engine.close();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void compute() {

		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein meanLevenshtein = engine.find(MeanLevenshtein.class);

		logger.info("Current doc class: {}", currentDocClass);

		// TODO convert to Stream?
		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue())).toList();
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");

		// Loop over all zone instances
		for (ZoneInstance zoneInstance : zoneInstances) {
			logger.info("=> Zone {}", zoneInstance);

			List<Float> meanLevDistances = new ArrayList<>();
			List<Float> probabilities = new ArrayList<>();

			// Loop over all filters
			for (ImgFilterInstance imgFilterInstance : imgFilterInstances) {
				int lev = 0; // contains the sum of all Levenshtein
								// distances for a given zone
				int count = 0; // contains the number of perfect matches

				// Loop over all documents in this class
				for (DocInstance docInstance : docInstances) {
					String realText;
					try {
						realText = (String) zoneText.getZoneText(docInstance, zoneInstance, realityInstance).getValue();
					} catch (NullPointerException e) {
						throw new RuntimeException("Unable to load the real values for the supervised training!", e);
					}
					ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
					if (zti == null) {
						logger.info(">> no text value for {}", imgFilterInstance.getValue());
						continue;
					}
					String text = (String) zti.getValue();
					// TODO : manipulate the Strings before comparison?
					// (remove spaces, etc.)
					int dist = Levenshtein.distance(text.replaceAll("[\n ,.]", "").trim(), realText.replaceAll("[\n ,.]", "").trim());

					count += (dist == 0) ? 1 : 0;
					lev += dist;
				}
				float probability = (float) count / (float) docInstances.size();
				float meanDistance = (float) lev / (float) docInstances.size();

				ScoreInstance scoreInstance = score.setScore(probability, zoneInstance, imgFilterInstance);
				MeanLevenshteinInstance meanLevenshteinInstance = meanLevenshtein.setMeanLev(meanDistance, scoreInstance);

				meanLevDistances.add(meanDistance);
				probabilities.add(probability);
			}
			engine.getCurrentCache().flush();
		}
	}

	private static void printBestResults(ZoneInstance zoneInstance, int limit) {
		Score score = engine.find(Score.class);
		MeanLevenshtein meanLevenshtein = engine.find(MeanLevenshtein.class);
		logger.info("=> Zone {} best filters:", zoneInstance.getValue());
		Comparator<Generic> sortByScore = (g1, g2) -> Float.compare((Float) g1.getValue(), (Float) g2.getValue());
		Comparator<Generic> sortByMeanLev = (g1, g2) -> Float.compare((Float) g1.getHolder(meanLevenshtein).getValue(), (Float) g2.getHolder(meanLevenshtein).getValue());
		zoneInstance.getHolders(score).stream().sorted(sortByScore.reversed().thenComparing(sortByMeanLev)).limit(limit).forEach(s -> {
			logger.info("{} (probability: {}, meanLev: {})", ((ScoreInstance) s).getImgFilter().getValue(), s.getValue(), s.getHolder(meanLevenshtein).getValue());
		});
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	private static void printResults(int limit) {
		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		Snapshot<ZoneInstance> zoneInstances = (Snapshot) currentDocClass.getHolders(engine.find(ZoneGeneric.class));
		zoneInstances.forEach(zi -> printBestResults(zi, limit));
	}

}
