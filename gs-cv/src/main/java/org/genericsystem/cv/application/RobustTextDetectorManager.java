package org.genericsystem.cv.application;

import java.util.ArrayList;

import org.genericsystem.cv.utils.NativeLibraryLoader;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Scalar;
import org.opencv.features2d.MSER;

public class RobustTextDetectorManager {

	static {
		NativeLibraryLoader.load();
	}

	private final Mat gray;
	private Mat mserMask;

	public RobustTextDetectorManager(Mat gray) {
		this.gray = gray;
	}

	public Mat getMserMask() {
		return mserMask != null ? mserMask : (mserMask = buildMserMask(gray));
	}

	private Mat buildMserMask(Mat gray) {
		MSER detector = MSER.create(3, 10, 2000, 0.25, 0.1, 100, 1.01, 0.03, 5);
		ArrayList<MatOfPoint> regions = new ArrayList<>();
		MatOfRect mor = new MatOfRect();
		detector.detectRegions(gray, regions, mor);
		Mat mserMask = new Mat(gray.size(), CvType.CV_8UC1, new Scalar(0));
		for (MatOfPoint mop : regions) {
			// Point[] array = mop.toArray();
			// double minX = Arrays.stream(array).mapToDouble(pt -> pt.x).min().getAsDouble();
			// double minY = Arrays.stream(array).mapToDouble(pt -> pt.y).min().getAsDouble();
			// double maxX = Arrays.stream(array).mapToDouble(pt -> pt.x).max().getAsDouble();
			// double maxY = Arrays.stream(array).mapToDouble(pt -> pt.y).max().getAsDouble();
			// Imgproc.rectangle(mserMask, new Point(minX, minY), new Point(maxX, maxY), new Scalar(Math.random() * 200 + 55), -1);
			// double color = Math.random() * 200 + 55;
			for (Point p : mop.toArray())
				mserMask.put((int) p.y, (int) p.x, 255);
		}
		return mserMask;
	}

}
