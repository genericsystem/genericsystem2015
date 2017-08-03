package org.genericsystem.cv.comparator;

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

/**
 * This class is used to serialize and de-serialize the parameters for the OCR, which need to be transfered across the Vertx event bus.
 * 
 * @author Pierrik Lassalas
 */
public class OcrParameters {
	public static final String P_FILE = "file";
	public static final String P_ZONES = "zones";
	public static final String P_IMG_FILTERS = "imgFilters";

	private static Logger log = LoggerFactory.getLogger(OcrParameters.class);

	private File file;
	private Zones zones;
	private List<ImgFilterFunction> imgFilterFunctions;

	public OcrParameters(File file, Zones zones, List<ImgFilterFunction> imgFilterFunctions) {
		this.file = file;
		this.zones = zones;
		this.imgFilterFunctions = imgFilterFunctions;
	}

	public OcrParameters(JsonObject jsonObject) {
		String filename = jsonObject.getString(P_FILE);
		JsonArray zoneArray = jsonObject.getJsonArray(P_ZONES);
		JsonArray filterArray = jsonObject.getJsonArray(P_IMG_FILTERS);
		List<Zone> zonesList = new ArrayList<>();
		List<ImgFilterFunction> imgFilterFunctions = new ArrayList<>();
		try {
			zoneArray.forEach(zone -> {
				try {
					zonesList.add((Zone) zone);
				} catch (Exception e) {
					log.debug("Unable to cast {} as Zone. Using Json.decodeValue instead", zone.toString());
					Zone z = Json.decodeValue(((JsonObject) zone).encode(), Zone.class);
					zonesList.add(z);
				}
			});
			filterArray.forEach(filter -> {
				try {
					imgFilterFunctions.add((ImgFilterFunction) filter);
				} catch (Exception e) {
					log.debug("Unable to cast {} as ImgFilterFunction. Using Json.decodeValue instead", filter);
					imgFilterFunctions.add(ImgFilterFunction.valueOf((String) filter));
				}
			});
		} catch (Exception e) {
			throw new IllegalStateException("An error has occured while reading the parameters", e);
		}
		this.file = new File(filename);
		this.zones = new Zones(zonesList);
		this.imgFilterFunctions = imgFilterFunctions;
	}

	public JsonObject toJson() {
		JsonObject jsonObject = new JsonObject();
		jsonObject.put(P_FILE, file.getAbsolutePath());
		jsonObject.put(P_ZONES, zones.getZones());
		jsonObject.put(P_IMG_FILTERS, imgFilterFunctions);
		return jsonObject;
	}

	public File getFile() {
		return file;
	}

	public Zones getZones() {
		return zones;
	}

	public List<ImgFilterFunction> getImgFilterFunctions() {
		return imgFilterFunctions;
	}

	public static void main(String[] args) {
		// Use an existing image
		Path imgPath = Paths.get(System.getProperty("user.dir") + "/../gs-cv/classes/id-fr-front/image6-1.png");
		final Path imgClassDirectory = imgPath.getParent();
		final List<ImgFilterFunction> imgFilterFunctions = FillModelWithData.getFilterFunctions();
		final Zones zones = Zones.load(imgClassDirectory.toString());

		// Get the parameters
		OcrParameters params = new OcrParameters(imgPath.toFile(), zones, imgFilterFunctions);
		System.out.println(params.toJson().encodePrettily());

		// Test the input
		OcrParameters test = new OcrParameters(params.toJson());
		System.out.println(test.getZones().getZones());
		System.out.println(test.getImgFilterFunctions());
	}

}
