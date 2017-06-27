package org.genericsystem.cv.comparator;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.Levenshtein;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.MeanLevenshtein;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.MeanLevenshtein.MeanLevenshteinInstance;
import org.genericsystem.cv.model.Score;
import org.genericsystem.cv.model.Score.ScoreInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.genericsystem.cv.model.ZoneText;

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

	private final static String docType = "id-fr-front";
	private final static Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/gs-cv_model/", Doc.class,
			ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);
	
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] mainArgs) {
		engine.newCache().start();
		compute();
		engine.close();
	}

	@SuppressWarnings({ "unchecked", "rawtypes" })
	public static void compute() {

		Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
		ImgFilter imgFilter = engine.find(ImgFilter.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		Score score = engine.find(Score.class);
		MeanLevenshtein meanLevenshtein = engine.find(MeanLevenshtein.class);

		System.out.println("Current doc class : " + currentDocClass);

		// TODO convert to Stream?
		List<DocInstance> docInstances = (List) currentDocClass.getHolders(engine.find(Doc.class)).toList();
		List<ZoneInstance> zoneInstances = (List) currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();
		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances()
				.filter(f -> !"reality".equals(f.getValue())).toList();
		ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");

		// Loop over all zone instances
		for (ZoneInstance zoneInstance : zoneInstances) {
			System.out.println("=> Zone " + zoneInstance);

			List<Float> meanLevDistances = new ArrayList<Float>();
			List<Float> probabilities = new ArrayList<Float>();

			// Loop over all filters
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
					// (remove spaces, etc.)
					int dist = Levenshtein.distance(text.replaceAll("[\n ,.]", "").trim(),
							realText.replaceAll("[\n ,.]", "").trim());

					count += (dist == 0) ? 1 : 0;
					lev += dist;
				}
				float probability = (float) count / (float) docInstances.size();
				float meanDistance = (float) lev / (float) docInstances.size();

				ScoreInstance scoreInstance = score.addScore(probability, zoneInstance, imgFilterInstance);
				MeanLevenshteinInstance meanLevenshteinInstance = meanLevenshtein.addMeanLev(meanDistance,
						scoreInstance);

				meanLevDistances.add(meanDistance);
				probabilities.add(probability);
			}
			for (int i = 0; i < imgFilterInstances.size(); i++) {
				System.out.println(imgFilterInstances.get(i) + " : " + meanLevDistances.get(i) + " (proba : "
						+ probabilities.get(i) + ")");
			}
			engine.getCurrentCache().flush();
		}

	}

}
