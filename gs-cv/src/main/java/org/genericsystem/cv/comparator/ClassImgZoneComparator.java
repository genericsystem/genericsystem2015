package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ImgFilter.ImgFilterInstance;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneGeneric.ZoneInstance;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.opencv.core.Size;

/**
 * Get the OCR text for all specified documents using pre-treated images.
 * 
 * A score gets computed based on trained data, and stored in csv files. It is recommended to use {@link FillModelWithData}, {@link SetRealValues} and {@link ComputeTrainedScores} instead.
 * 
 * @author Pierrik Lassalas
 *
 */
@Deprecated
public class ClassImgZoneComparator {

	private final static String imgClassDirectory = "classes/id-fr-front";
	private final static Engine engine = new Engine(System.getenv("HOME") + "/genericsystem/gs-cv_model/", Doc.class, DocClass.class, ZoneGeneric.class, ZoneText.class, ImgFilter.class);

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		// Start the engine
		engine.newCache().start();
		compute();
		engine.close();
	}

	protected static void compute() {

		ImgClass imgClass = ImgClass.fromDirectory(imgClassDirectory);

		Zones zones;
		try {
			zones = Zones.load(imgClassDirectory);
		} catch (RuntimeException e) {
			System.out.println("could not load accurate zones");
			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(20, 3)));
			zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
		}

		final Zones zones2 = zones;

		/*
		 * Get the real String value from GS (filtername = "reality")
		 */
		Generic doc = engine.find(Doc.class);
		DocClass docClass = engine.find(DocClass.class);
		ZoneText zoneText = engine.find(ZoneText.class);
		ImgFilter imgFilter = engine.find(ImgFilter.class);

		// Get the current class
		DocClassInstance docClassInstance = docClass.getDocClass(imgClassDirectory.replace("classes/", ""));

		// Compute the scores for each image in the "/ref" sub-directory
		Arrays.asList(new File(imgClassDirectory + "/ref/").listFiles()).stream().filter(img -> img.getName().endsWith(".png")).forEach(file -> {
			System.out.println("File : " + file.getName());
			// Create a Map containing both the img and the name of the
			// filter
			Map<Img, String> imgFiltersMap = new HashMap<>();
			imgFiltersMap.put(new Img(file.getPath()), "original");
			Arrays.asList(new File(imgClassDirectory + "/mask/" + file.getName().replace(".png", "")).listFiles()).stream().filter(img -> img.getName().endsWith(".png")).forEach(img -> {
				imgFiltersMap.put(new Img(img.getPath()), img.getName().replace(file.getName().replace(".png", ""), "").substring(1).replace(".png", ""));
			});

			for (Zone zone : zones2) {
				System.out.println("Zone n°" + zone.getNum());

				// Get the document instance
				DocInstance docInstance = (DocInstance) docClassInstance.getHolder(doc, file.getName());
				ZoneInstance zoneInstance = docClassInstance.getZone(zone.getNum());
				ImgFilterInstance imgFilterInstance = (ImgFilterInstance) imgFilter.getInstance("reality");
				// Get the real text from GS
				ZoneTextInstance zoneTextInstance = (ZoneTextInstance) zoneText.getInstance(docInstance, zoneInstance, imgFilterInstance);
				String realText = zoneTextInstance.getValue().toString();
				System.out.println("> real text : " + realText);

				// ZoneScorerMap scorer =
				// zone.newUnsupervisedScorerMap(file.getName(),
				// map.entrySet().stream());
				ZoneScorerMap scorer = zone.newSupervisedScorerMap(file.getName(), realText, imgFiltersMap.entrySet().stream());
			}
		});
	}
}
