package org.genericsystem.cv.comparator;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;

import io.vertx.core.json.JsonObject;

/**
 * This class is used to serialize and de-serialize the parameters for the OCR, which need to be transfered across the Vertx event bus.
 * 
 * @author Pierrik Lassalas
 */
@SuppressWarnings({ "unchecked", "rawtypes" })
public class OcrParameters {
	public static final String P_FILE = "file";
	public static final String P_ZONES = "zones";
	public static final String P_IMG_FILTERS = "imgFilters";

	private File file;
	private Zones zones;
	private Map<String, Function<Img, Img>> imgFilters;

	public OcrParameters(File file, Zones zones, Map<String, Function<Img, Img>> imgFilters) {
		this.file = file;
		this.zones = zones;
		this.imgFilters = imgFilters;
	}

	public OcrParameters(JsonObject jsonObject) {
		try {
			String filename = jsonObject.getString(P_FILE);
			List<Zone> zones = new ArrayList<>();
			jsonObject.getJsonArray(P_ZONES).forEach(zone -> zones.add((Zone) zone));
			Map<String, Function<Img, Img>> imgFilters = (Map) jsonObject.getJsonObject(P_IMG_FILTERS).getMap();
			this.file = new File(filename);
			this.zones = new Zones(zones);
			this.imgFilters = imgFilters;
		} catch (Exception e) {
			throw new IllegalStateException("An error has occured while reading the parameters", e);
		}
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();
		jsonObject.put(P_FILE, file.getAbsolutePath());
		jsonObject.put(P_ZONES, zones.getZones());
		jsonObject.put(P_IMG_FILTERS, imgFilters);
		return jsonObject;
	}

	public File getFile() {
		return file;
	}

	public Zones getZones() {
		return zones;
	}

	public Map<String, Function<Img, Img>> getImgFilters() {
		return imgFilters;
	}
}
