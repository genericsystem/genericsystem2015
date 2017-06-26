package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.genericsystem.cv.Img;
import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.kernel.Engine;
import org.opencv.core.Core;
import org.opencv.core.Size;
import org.opencv.imgcodecs.Imgcodecs;

public class ClassImgFieldsDetectorComparator {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String imgClassDirectory = "classes/id-fr-front";
	
	private static Engine engine;

	public static void main(String[] args) {
		// Start the engine
		engine = new Engine(System.getenv("HOME") + "/genericsystem/gs-cv_model/", Doc.class, DocClass.class,
				ZoneGeneric.class, ZoneText.class, ImgFilter.class);
		engine.newCache().start();
		compute();
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

		// Compute the scores for each image in the "/ref" sub-directory
		Arrays.asList(new File(imgClassDirectory + "/ref/").listFiles()).stream().filter(img -> img.getName().endsWith(".png"))
				.forEach(file -> {
					System.out.println("File : " + file.getName());
					// Create a Map containing both the img and the name of the
					// filter
					Map<Img, String> map = new HashMap<>();
					map.put(new Img(Imgcodecs.imread(file.getPath())), "original");
					Arrays.asList(
							new File(imgClassDirectory + "/mask/" + file.getName().replace(".png", "")).listFiles())
							.stream().filter(img -> img.getName().endsWith(".png")).forEach(img -> {
								map.put(new Img(Imgcodecs.imread(img.getPath())),
										img.getName().replace(file.getName().replace(".png", ""), "").substring(1)
												.replace(".png", ""));
							});

					for (Zone zone : zones2) {
						System.out.println("Zone n°" + zone.getNum());
//						ZoneScorerMap2 scorer = zone.newUnsupervisedScorerMap(file.getName(), map.entrySet().stream());
						ZoneScorerMap2 scorer = zone.newSupervisedScorerMap(file.getName(), map.entrySet().stream());
					}
				});

		// Call the garbage collector to free the resources
		System.gc();

	}
}
