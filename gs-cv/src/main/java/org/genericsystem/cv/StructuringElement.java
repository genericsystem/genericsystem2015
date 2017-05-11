package org.genericsystem.cv;

import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;

public class StructuringElement extends Img {

	public StructuringElement(int morph, Size size) {
		super(Imgproc.getStructuringElement(morph, size));
	}
}
