package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

public class Zones {
	private final List<Zone> zones;

	public static Zones get(Img img, double minArea) {
		return new Zones(img, minArea);
	}

	public static Zones get(Img img, double minArea, double dx, double dy) {
		return new Zones(img, minArea).adjust(dx, dy, img.width(), img.height());
	}

	private Zones adjust(double dx, double dy, int width, int height) {
		return new Zones(zones.stream().map(zone -> zone.adjustRect(dx, dy, width, height)).collect(Collectors.toList()));
	}

	public Zones(List<Zone> zonesList) {
		this.zones = zonesList;
	}

	public Zones(Img gray, double minArea) {
		this.zones = new ArrayList<>();
		List<MatOfPoint> contours = gray.findContours(new Img[1], Imgproc.RETR_EXTERNAL, Imgproc.CHAIN_APPROX_SIMPLE);
		for (int i = 0; i < contours.size(); i++) {
			MatOfPoint contour = contours.get(i);
			double contourarea = Imgproc.contourArea(contour);
			if (contourarea > minArea) {
				Rect rect = Imgproc.boundingRect(contour);
				if (rect.width >= rect.height)
					zones.add(new Zone(rect));
			}
		}
	}

	public List<Zone> get() {
		return zones;
	}

	public void draw(Img img, Scalar scalar, int thickness) {
		zones.forEach(adjusted -> adjusted.draw(img, scalar, thickness));
	}
}
