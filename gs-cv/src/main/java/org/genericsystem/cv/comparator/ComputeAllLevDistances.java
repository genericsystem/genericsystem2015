package org.genericsystem.cv.comparator;

import java.lang.invoke.MethodHandles;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.LevDistance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.cv.utils.Levenshtein;
import org.genericsystem.kernel.Engine;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The ComputeAllLevDistances class computes the {@link LevDistance} between two {@link ZoneText}. The data is retrieved from GS, and stored in GS.
 * 
 * @author Pierrik Lassalas
 */
public class ComputeAllLevDistances {

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private final static String docType = "id-fr-front";
	private static final String gsPath = System.getenv("HOME") + "/genericsystem/gs-cv_model/";
	private final static Engine engine = new Engine(gsPath, Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class, LevDistance.class);

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
		LevDistance levDistance = engine.find(LevDistance.class);

		logger.info("Current doc class: {}", currentDocClass);

		Snapshot<DocInstance> docInstances = (Snapshot) currentDocClass.getHolders(engine.find(Doc.class));
		Snapshot<ZoneInstance> zoneInstances = (Snapshot) currentDocClass.getHolders(engine.find(ZoneGeneric.class));

		List<ImgFilterInstance> imgFilterInstances = (List) imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue())).toList();

		// Loop over all documents in this class
		docInstances.forEach(docInstance -> {
			logger.info("=> Document: {}", docInstance);

			// Loop over all zone instances
			zoneInstances.forEach(zoneInstance -> {
				logger.info("  => Zone {}", zoneInstance);

				// Loop over all filters
				for (int i = 0; i < imgFilterInstances.size(); ++i) {
					logger.info("    => Filter {}", imgFilterInstances.get(i));
					ZoneTextInstance zoneText1 = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstances.get(i));
					for (int j = i; j < imgFilterInstances.size(); ++j) {
						int dist = 0;
						if (i == j) {
							levDistance.setLevDistance(0, zoneText1, zoneText1);
							break;
						}
						ZoneTextInstance zoneText2 = zoneText.getZoneText(docInstance, zoneInstance, imgFilterInstances.get(j));

						String text1 = (String) zoneText1.getValue();
						String text2 = (String) zoneText2.getValue();
						dist = Levenshtein.distance(text1.trim(), text2.trim());
						levDistance.setLevDistance(dist, zoneText1, zoneText2);
						levDistance.setLevDistance(dist, zoneText2, zoneText1);
					}
				}
				engine.getCurrentCache().flush();
			});
		});
		engine.getCurrentCache().flush();
	}
}
