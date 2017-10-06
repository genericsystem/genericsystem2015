package org.genericsystem.cv.utils;

import java.lang.invoke.MethodHandles;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.cv.Img;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ClassifierUsingFields {

	static {
		NativeLibraryLoader.load();
	}

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	private static final double closedImgSizeFactor = 1.2E-6;
	private static final double minAreaFactor = 3E-5;

	// For the verticles
	public static Path classify(Path imgPath) {
		Path classifiedImgPath = null;
		// Add some logic here...
		return classifiedImgPath;
	}

	public static Img getFieldsDrawn(final Img img, final Scalar scalar, final int thickness) {
		Img imgCopy = new Img(img.getSrc(), true);
		List<Rect> rectangles = detectRects(imgCopy);
		rectangles.forEach(rect -> drawSingleRectangle(imgCopy.getSrc(), rect, scalar, thickness));
		return imgCopy;
	}

	public static Img getBinary(final Img img) {
		return getClosedImage(img);
	}

	public static List<Rect> detectRects(final Img img) {
		Img closed = getClosedImage(img); // getClosedImage(img, 7, 3, new Size(9, 1));
		List<MatOfPoint> contours = getContours(closed);
		double minArea = minAreaFactor * img.size().area();
		List<Rect> res = contours.stream().filter(contour -> Imgproc.contourArea(contour) > minArea).map(Imgproc::boundingRect).filter(rect -> rect.height > 3 && rect.width > 3).collect(Collectors.toList());
		contours.forEach(MatOfPoint::release);
		return res;
	}

	// This function modifies the Mat mat
	private static void drawSingleRectangle(Mat mat, final Rect rect, final Scalar scalar, final int thickness) {
		Imgproc.rectangle(mat, rect.tl(), rect.br(), scalar, thickness);
	}

	private static Img getClosedImage(final Img img) {
		double size = (closedImgSizeFactor * img.size().area());
		// Round the size factor to the nearest odd int
		size = 2 * (Math.floor(size / 2)) + 1;
		return img.bilateralFilter(10, 80, 80).bgr2Gray().grad(2.0d, 2.0d).thresHold(0, 255, Imgproc.THRESH_BINARY_INV + Imgproc.THRESH_OTSU).bitwise_not().morphologyEx(Imgproc.MORPH_CLOSE, Imgproc.MORPH_ELLIPSE, new Size(size, size / 5 + 1));
	}

	private static List<MatOfPoint> getContours(final Img closed) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(closed.getSrc(), contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		return contours;
	}

}
