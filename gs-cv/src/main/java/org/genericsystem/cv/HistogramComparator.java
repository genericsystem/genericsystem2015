package org.genericsystem.cv;

import java.util.stream.Collectors;

import org.opencv.core.Core;

public class HistogramComparator {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	private final static String classImgRepertory = "png";

	public static void main(String[] args) {
		Tools.classImgsStream(classImgRepertory, "image-3.png", "image3-3.png", "image3-1.png", "image3-4.png").forEach(
				img -> System.out.println("Index of most similar image : " + img.findBestHisto(Tools.classImgsStream(classImgRepertory, "image3-4.png", "image3-1.png", "image3-3.png", "image-3.png").collect(Collectors.toList()))));

	}
}
