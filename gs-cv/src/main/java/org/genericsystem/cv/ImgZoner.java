package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.MatOfPoint;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class ImgZoner {
	private List<Zone> zones = new ArrayList<>();

	public static List<Zone> getZones(Img img, double minArea) {
		return new ImgZoner(img, minArea).getZones();
	}

	public static List<Zone> getAdjustedZones(Img img, double minArea, double dx, double dy) {
		return getZones(img, minArea).stream().map(zone -> zone.adjustRect(dx, dy, img.width(), img.height())).collect(Collectors.toList());
	}

	private ImgZoner(Img gray, double minArea) {

		List<MatOfPoint> contours = gray.findContours(new Img[1], Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
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

	public static void drawAdjustedZones(Img img, double minArea, double dx, double dy, Scalar scalar, int thickness) {
		getAdjustedZones(img, minArea, dx, dy).forEach(adjusted -> adjusted.draw(img, scalar, thickness));
	}

	public static void drawZones(Img imageToZone, double minArea, Scalar scalar, int thickness) {
		getZones(imageToZone, minArea).forEach(adjusted -> adjusted.draw(imageToZone, scalar, thickness));
	}

	public static void drawZones(Img imageToZone, Img imageToDraw, double minArea, Scalar scalar, int thickness) {
		getZones(imageToZone, minArea).forEach(adjusted -> adjusted.draw(imageToDraw, scalar, thickness));
	}

}
