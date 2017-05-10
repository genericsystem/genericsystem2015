package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class ImgZoner {
	private List<Zone> zones = new ArrayList<>();

	public static List<Zone> getZones(Mat imgSrc, double minArea) {
		return new ImgZoner(imgSrc, minArea).getZones();
	}

	public static List<Zone> getAdjustedZones(Mat imgSrc, double minArea, double dx, double dy) {
		return getZones(imgSrc, minArea).stream().map(zone -> zone.adjustRect(dx, dy, imgSrc.width(), imgSrc.height())).collect(Collectors.toList());
	}

	private ImgZoner(Mat imgSrc, double minArea) {
		Mat gray = new Mat();
		Imgproc.cvtColor(imgSrc, gray, Imgproc.COLOR_BGR2GRAY);
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(gray, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		// double minArea = 1;
		Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
		for (int i = 0; i < contours.size(); i++) {
			Imgproc.drawContours(imgSrc, contours, i, new Scalar(0, 0, 255), -1);
		}

		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > minArea)
				zones.add(new Zone(Imgproc.boundingRect(contour)));

		}
	}

	public List<Zone> getZones() {
		return zones;
	}

	public static void drawAdjustedZones(Mat imageToZone, double minArea, double dx, double dy, Scalar scalar, int thickness) {
		getAdjustedZones(imageToZone, minArea, dx, dy).forEach(adjusted -> adjusted.draw(imageToZone, scalar, thickness));
	}

	public static void drawZones(Mat imageToZone, double minArea, Scalar scalar, int thickness) {
		getZones(imageToZone, minArea).forEach(adjusted -> adjusted.draw(imageToZone, scalar, thickness));
	}

}
