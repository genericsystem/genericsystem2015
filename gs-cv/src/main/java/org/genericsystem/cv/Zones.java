package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.opencv.core.MatOfPoint;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.imgproc.Imgproc;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;

public class Zones {
	private List<Zone> zones;
	private static final ObjectMapper mapper = new ObjectMapper();

	public static Zones get(Img img, double minArea) {
		return new Zones(img.channels() == 1 ? img : img.gray(), minArea);
	}

	public static Zones get(Img img, double minArea, double dx, double dy) {
		return new Zones(img.channels() == 1 ? img : img.gray(), minArea).adjust(dx, dy, img.width(), img.height());
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

	public List<Zone> getZones() {
		return zones;
	}

	public void save(File file) {
		file.getParentFile().mkdirs();
		mapper.enable(SerializationFeature.INDENT_OUTPUT);
		try {
			mapper.writeValue(file, this);
			System.out.println("Zones saved in " + file.getAbsolutePath());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}

	}

	public Zones load(File file) throws JsonParseException, JsonMappingException, IOException {
		return mapper.readValue(file, Zones.class);
	}

}
