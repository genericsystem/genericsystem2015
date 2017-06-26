package org.genericsystem.cv.comparator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.genericsystem.common.Generic;
import org.genericsystem.common.Root;
import org.genericsystem.cv.Levenshtein;
import org.genericsystem.cv.comparator.ComputeScores.ComputeScript;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.annotations.RunScript;
import org.genericsystem.reactor.appserver.ApplicationServer;
import org.genericsystem.reactor.appserver.Script;
import org.genericsystem.reactor.gscomponents.RootTagImpl;
import org.opencv.core.Core;

@RunScript(ComputeScript.class)
@DependsOnModel({ Doc.class, ImgFilter.class, ZoneGeneric.class, ZoneText.class })
public class ComputeScores extends RootTagImpl {

	public static void main(String[] mainArgs) {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
		ApplicationServer.startSimpleGenericApp(mainArgs, ComputeScores.class, "/gs-cv_model");
	}

	public static class ComputeScript implements Script {

		@Override
		public void run(Root engine) {

			String docType = "id-fr-front";

			Generic currentDocClass = engine.find(DocClass.class).getInstance(docType);
			System.out.println("Current doc class : " + currentDocClass);
			List<Generic> docInstances = currentDocClass.getHolders(engine.find(Doc.class)).toList();

			ImgFilter imgFilter = engine.find(ImgFilter.class);
			List<Generic> imgFilterInstances = imgFilter.getInstances().filter(f -> !"reality".equals(f.getValue()))
					.toList();
			ImgFilterInstance realityInstance = imgFilter.getImgFilter("reality");

			List<Generic> zoneInstances = currentDocClass.getHolders(engine.find(ZoneGeneric.class)).toList();

			ZoneText zoneText = engine.find(ZoneText.class);

			for (Generic zoneInstance : zoneInstances) {
				System.out.println("=> Zone " + zoneInstance);
				List<Float> meanLevDistances = new ArrayList<Float>();
				for (Generic imgFilterInstance : imgFilterInstances) {
					int lev = 0;
					for (Generic docInstance : docInstances) {
						String realText = (String) zoneText
								.getZoneText((DocInstance) docInstance, (ZoneInstance) zoneInstance, realityInstance)
								.getValue();
						String text = (String) zoneText.getZoneText((DocInstance) docInstance,
								(ZoneInstance) zoneInstance, (ImgFilterInstance) imgFilterInstance).getValue();
						lev += Levenshtein.distance(text, realText);
					}
					meanLevDistances.add((float) lev / (float) docInstances.size());
				}
				List<Float> meanLevDistances2 = meanLevDistances;
				Collections.sort(meanLevDistances2);
				for (int i = 0; i < imgFilterInstances.size(); i++) {
					if (meanLevDistances.get(i) <= meanLevDistances2.get(2))
						System.out.println(imgFilterInstances.get(i) + " : " + meanLevDistances.get(i));
				}
			}

		}

	}

}
