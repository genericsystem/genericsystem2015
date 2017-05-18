package org.genericsystem.cv;

import java.io.IOException;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.opencv.core.Core;

public class HistogramComparator {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "png";

	public static void main(String[] args) {
		List<Path> directories = new ArrayList<>();
		List<Img> imgs = new ArrayList<>();
		try (DirectoryStream<Path> directoryStream = Files.newDirectoryStream(Paths.get(".", "classes"), Files::isDirectory)) {
			for (Path path : directoryStream) {
				directories.add(path);
				ImgClass imgClass = new ImgClass(path.toString());
				imgs.add(imgClass.getClassModel() != null ? imgClass.getClassModel() : imgClass.getMean());
				System.gc();
				System.runFinalization();
			}
		} catch (IOException e) {
			throw new IllegalStateException(e);
		}
		for (int i = 0; i < directories.size(); i++) {
			Path path = directories.get(i);
			System.out.println("\n======================= Images of class: " + path);
			Tools.classImgsStream(path.toString()).forEach(img -> {
				System.out.println("Best histogram: " + directories.get(img.findBestHisto(imgs)));
				System.gc();
				System.runFinalization();
			});
		}
	}
}
