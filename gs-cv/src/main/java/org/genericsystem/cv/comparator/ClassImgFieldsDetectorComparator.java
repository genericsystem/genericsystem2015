package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Stream;

import org.genericsystem.cv.ImgClass;
import org.genericsystem.cv.Tools;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;
import org.opencv.core.Core;
import org.opencv.core.Size;

public class ClassImgFieldsDetectorComparator {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String imgClassDirectory = "classes/id-fr-front";

	public static void main(String[] args) {
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

		int i = 0;
		
		// TODO lazily load the files to avoid memory overload
		for (File file : new File(imgClassDirectory).listFiles())
			if (file.getName().endsWith(".png")) {
				// Create the Map 
//				Map<File, String> map = Arrays.asList(new File(imgClassDirectory).listFiles()).
//						stream().filter(img -> img.getName().equals(file.getName()))
						
				
				System.out.println("File : " + file.getName());
				if (i++ > 0)
					continue;
				for (Zone zone : zones) {
					System.out.println("Zone n°" + zone.getNum());
					// TODO create a new Stream with a wrapper containing the file and the name of the filter 
					ZoneScorer2 scorer = zone.newUnsupervisedScorer2(file, Stream.concat(
							Tools.classImgsStream(imgClassDirectory, file.getName()),
							Tools.classImgsStream(imgClassDirectory + "/mask/" + file.getName().replace(".png", ""))));
				}
			}
	}
}
