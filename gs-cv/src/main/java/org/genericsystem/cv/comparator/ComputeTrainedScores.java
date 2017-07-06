package org.genericsystem.cv.comparator;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Levenshtein;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ComputeTrainedScores class computes the {@link Score} and the
 * {@link MeanLevenshtein} for each zone and each filter.
 * 
 * The data is retrieved from GS, and stored in GS.
 * 
 * @author Pierrik Lassalas
 *
 */
public class ComputeTrainedScores {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model2/";
	private static Logger log = LoggerFactory.getLogger(ComputeTrainedScores.class);

	public static void main(String[] mainArgs) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class,
				Score.class, MeanLevenshtein.class);

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
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein meanLevenshtein = engine.find(MeanLevenshtein.class);

		log.info("Current doc class : {} ", currentDocClass);

		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances()
				.filter(f -> !"reality".equals(f.getValue())).toList();
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");

		// Loop over all zone instances
		for (ZoneInstance zoneInstance : zoneInstances) {
			log.info("=> Zone {}", zoneInstance);

			List<Float> meanLevDistances = new ArrayList<Float>();
			List<Float> probabilities = new ArrayList<Float>();

			// Loop over all filter instances
			for (ImgFilterInstance imgFilterInstance : imgFilterInstances) {
				int lev = 0; // contains the sum of all Levenshtein
								// distances for a given zone
				int count = 0; // contains the number of perfect matches

				// Loop over all documents in this class
				for (DocInstance docInstance : docInstances) {
					String realText = (String) zoneText.getZoneText(docInstance, zoneInstance, realityInstance)
							.getValue();
					String text = (String) zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance)
							.getValue();
					// TODO : manipulate the Strings before comparison?
					int dist = Levenshtein.distance(text.replaceAll("[\n ,.]", "").trim(),
							realText.replaceAll("[\n ,.]", "").trim());

					count += (dist == 0) ? 1 : 0;
					lev += dist;
				}
				float probability = (float) count / (float) docInstances.size();
				float meanDistance = (float) lev / (float) docInstances.size();

				ScoreInstance scoreInstance = score.setScore(probability, zoneInstance, imgFilterInstance);
				meanLevenshtein.setMeanLev(meanDistance, scoreInstance);
				engine.getCurrentCache().flush();

				meanLevDistances.add(meanDistance);
				probabilities.add(probability);
			}
			engine.getCurrentCache().flush();
			
			for (int i = 0; i < imgFilterInstances.size(); i++) {
				log.info("{}: (proba: {})", imgFilterInstances.get(i), meanLevDistances.get(i), probabilities.get(i));
			}
		}

	}

}
