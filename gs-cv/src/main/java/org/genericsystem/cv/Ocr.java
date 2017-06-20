package org.genericsystem.cv;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.text.OCRTesseract;

public class Ocr {
	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static OCRTesseract instance = OCRTesseract.create("/usr/share/tesseract-ocr/4.00/", "fra", "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM0123456789.-,<'", 1, 7);

	public static void main(String[] args) {
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(Paths.get(".", "classes"), Files::isDirectory)) {
			for (Path directoryPath : directoryStream) {
				System.out.println("--- Images of class: " + directoryPath);
				try (DirectoryStream<Path> imagesStream = Files.newDirectoryStream(directoryPath, Files::isRegularFile)) {
					imagesStream.forEach(path -> {
						System.out.println("------ Image: " + path);
						ocrClassifiedImage(path);
					});
				}
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
	}

	public static void ocrClassifiedImage(Path imagePath) {
		Path imgClassDirectory = imagePath.getParent();
		Path zonesFile = imgClassDirectory.resolve("zones/zones.json");
		Zones zones = null;
		if (zonesFile.toFile().exists()) {
			System.out.println("Precomputed zones found, file: " + zonesFile);
			zones = Zones.load(zonesFile.toFile());
		}
		if (zones == null) {
			ImgClass imgClass = ImgClass.fromDirectory(imgClassDirectory.toString());
			imgClass.addMapper(img -> img.eraseCorners(0.1).dilateBlacks(86, 255, 76, new Size(15, 3)));
			zones = Zones.get(imgClass.getClosedVarianceZones(new Size(9, 10)), 300, 6, 6);
		}
		for (Zone zone : zones) {
			ZoneScorer scorer = zone.newUnsupervisedScorer(Tools.classImgsStream(imgClassDirectory + "/mask/" + imagePath.getFileName().toString().replace(".png", "")));
			System.out.println("Image " + imagePath + ", found text: " + scorer.getBestText() + " " + Math.floor((scorer.getBestScore() * 10000)) / 100 + "%");
		}
		System.gc();
		System.runFinalization();
	}

	public static String doWork(Mat mat) {
		return doWork(mat, 0);
	}

	public static String doWork(Mat mat, int minConfidence) {
		return instance.run(mat, minConfidence, 1);
	}
}
