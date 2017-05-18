package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.MatOfPoint;
import org.opencv.core.MatOfPoint2f;
import org.opencv.core.RotatedRect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class RotatedZones {
	private final List<RotatedZone> zones;

	public static RotatedZones get(Img img, double minArea) {
		return new RotatedZones(img, minArea);
	}

	public static RotatedZones get(Img img, double minArea, double dx, double dy) {
		return new RotatedZones(img, minArea).adjust(dx, dy, img.width(), img.height());
	}

	private RotatedZones adjust(double dx, double dy, int width, int height) {
		return new RotatedZones(zones.stream().map(zone -> zone.adjustRect(dx, dy, width, height)).collect(Collectors.toList()));
	}

	public RotatedZones(List<RotatedZone> zonesList) {
		this.zones = zonesList;
	}

	public RotatedZones(Img gray, double minArea) {
		this.zones = new ArrayList<>();
		List<MatOfPoint> contours = gray.findContours(new Img[1], Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > minArea) {
				MatOfPoint2f contour2F = new MatOfPoint2f(contour.toArray());
				RotatedRect rect = Imgproc.minAreaRect(contour2F);
				if (rect.size.width >= rect.size.height)
					zones.add(new RotatedZone(rect));
			}
		}
	}

	public List<RotatedZone> get() {
		return zones;
	}

	public void draw(Img img, Scalar scalar, int thickness) {
		zones.forEach(adjusted -> adjusted.draw(img, scalar, thickness));
	}
}
