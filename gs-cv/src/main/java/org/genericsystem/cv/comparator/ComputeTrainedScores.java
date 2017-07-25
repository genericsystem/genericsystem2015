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
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ComputeTrainedScores class computes the {@link Score} and the {@link MeanLevenshtein} for each zone and each filter.
 * 
 * The data is retrieved from GS, and stored in GS.
 * 
 * @author Pierrik Lassalas
 *
 */
public class ComputeTrainedScores {

	private static final Logger log = LoggerFactory.getLogger(ComputeTrainedScores.class);
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model3/";

	public static void main(String[] mainArgs) {
		final Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, Score.class, MeanLevenshtein.class);

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
				.filter(f -> !"reality".equals(f.getValue()) && !"best".equals(f.getValue())).toList();
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
				int count = 0; // contains the number of "perfect" matches
				int totalDocs = docInstances.size(); // contains the number of
														// documents

				// Loop over all documents in this class
				for (DocInstance docInstance : docInstances) {
					ZoneTextInstance realZti = zoneText.getZoneText(docInstance, zoneInstance, realityInstance);
					// Do not attempt the computation if the document was not
					// supervised
					if (realZti == null) {
						log.debug("Document {} on zone {} was not supervised (passed)", docInstance.getValue(),
								zoneInstance.getValue());
						totalDocs--; // Decrement the total size, since this
										// value will not be accounted for in
										// the statistics
					} else {
						String realText = (String) realZti.getValue();
						ZoneTextInstance zti = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstance);
						// Do not proceed if the zoneText does not exists (i.e.,
						// the algorithm was not applied to this image)
						if (zti == null) {
							log.debug("No text found for {} => zone n°{}, {}", docInstance.getValue(),
									zoneInstance.getValue(), imgFilterInstance.getValue());
							totalDocs--; // Decrement the total size, since this
											// value will not be accounted for
											// in the statistics
						} else {
							String text = (String) zti.getValue();
							// TODO : manipulate the Strings before comparison?
							int dist = Levenshtein.distance(text.replaceAll("[\n ,.]", "").trim(),
									realText.replaceAll("[\n ,.]", "").trim());

							count += (dist == 0) ? 1 : 0;
							lev += dist;
						}
					}
				}
				if (totalDocs > 0) {
					float probability = (float) count / (float) totalDocs;
					float meanDistance = (float) lev / (float) totalDocs;

					ScoreInstance scoreInstance = score.setScore(probability, zoneInstance, imgFilterInstance);
					meanLevenshtein.setMeanLev(meanDistance, scoreInstance);
					engine.getCurrentCache().flush();

					meanLevDistances.add(meanDistance);
					probabilities.add(probability);
				} else {
					log.error("An error has occured while processing the score computation of zone n°{} (class: {})",
							zoneInstance.getValue(), docType);
				}
			}
			engine.getCurrentCache().flush();

			for (int i = 0; i < imgFilterInstances.size(); i++) {
				log.info("{}: {} (meanLev: {})", imgFilterInstances.get(i), probabilities.get(i),
						meanLevDistances.get(i));
			}
		}

	}

}
