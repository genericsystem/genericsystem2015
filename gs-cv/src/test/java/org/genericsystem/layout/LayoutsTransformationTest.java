package org.genericsystem.layout;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zone;
import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;

public class LayoutsTransformationTest {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		new LayoutsTransformationTest().testScale();
	}

	public void testScale() {
		Img binary = new Img(new Mat(new Size(800, 600), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(200, 200), new Point(300, 300))).draw(binary, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(400, 400), new Point(500, 500))).draw(binary, new Scalar(0), -1);

		Layout layout = binary.buildLayout(new Size(0.04, 0.008), 8);
		System.out.println(layout.recursiveToString());

		binary = new Img(new Mat(new Size(800, 600), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(250, 200), new Point(350, 300))).draw(binary, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(450, 400), new Point(550, 500))).draw(binary, new Scalar(0), -1);

		layout = binary.buildLayout(new Size(0.04, 0.008), 8);
		System.out.println(layout.recursiveToString());

		binary = new Img(new Mat(new Size(160, 120), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(40, 40), new Point(60, 60))).draw(binary, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(80, 80), new Point(100, 100))).draw(binary, new Scalar(0), -1);

		layout = binary.buildLayout(new Size(0.04, 0.008), 8);
		System.out.println(layout.recursiveToString());
	}
}
