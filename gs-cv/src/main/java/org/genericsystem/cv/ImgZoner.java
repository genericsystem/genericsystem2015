package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import org.opencv.core.Mat;
import org.opencv.core.MatOfPoint;
import org.opencv.imgproc.Imgproc;

public class ImgZoner {
	private List<Zone> zones = new ArrayList<Zone>();

	public static List<Zone> getZones(Mat imgSrc) {
		return new ImgZoner(imgSrc).getZones();
	}

	private ImgZoner(Mat imgSrc) {
		List<MatOfPoint> contours = new ArrayList<>();
		Imgproc.findContours(imgSrc, contours, new Mat(), Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		double minArea = 500;
		Collections.sort(contours, (c1, c2) -> Double.compare(Imgproc.contourArea(c2), Imgproc.contourArea(c1)));
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

}
