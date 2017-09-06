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
import org.testng.annotations.Test;

public class LayoutsTransformationTest {

	static {
		System.loadLibrary(Core.NATIVE_LIBRARY_NAME);
	}

	public static void main(String[] args) {
		new LayoutsTransformationTest().testScale();
	}

	@Test
	public void testScale() {
		Img binary1 = new Img(new Mat(new Size(800, 600), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(200, 200), new Point(299, 299))).draw(binary1, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(400, 400), new Point(499, 499))).draw(binary1, new Scalar(0), -1);

		Layout layout1 = binary1.buildLayout(new Size(0.04, 0.008), 8);
		System.out.println(layout1.recursiveToString());

		Img binary2 = new Img(new Mat(new Size(800, 600), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(250, 200), new Point(349, 299))).draw(binary2, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(450, 400), new Point(549, 499))).draw(binary2, new Scalar(0), -1);

		Layout layout2 = binary2.buildLayout(new Size(0.04, 0.008), 8);
		System.out.println(layout2.recursiveToString());

		Img binary3 = new Img(new Mat(new Size(160, 120), CvType.CV_8U, new Scalar(255)), false);
		new Zone(0, new Rect(new Point(40, 40), new Point(59, 59))).draw(binary3, new Scalar(0), -1);
		new Zone(0, new Rect(new Point(80, 80), new Point(99, 99))).draw(binary3, new Scalar(0), -1);

		Layout layout3 = binary3.buildLayout(new Size(0.04, 0.02), 8);
		System.out.println(layout3.recursiveToString());

		// Close the images
		binary1.close();
		binary2.close();
		binary3.close();
	}
}
