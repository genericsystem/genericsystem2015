package org.genericsystem.cv.utils;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.List;

import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class ZonesJsonConverterForUID {

	private static final String docClass = "passport-fr";
	private static final String filename = System.getProperty("user.dir") + "/../gs-cv/classes/" + docClass + "/zones/zones-old.json";
	private static final String newFilename = System.getProperty("user.dir") + "/../gs-cv/classes/" + docClass + "/zones/zones.json";

	public static void main(String[] args) {

		// Read the initial file into a StringBuffer
		StringBuffer sb = new StringBuffer();
		try {
			for (String string : Files.readAllLines(Paths.get(filename))) {
				sb.append(string);
			}
		} catch (IOException e) {
			e.printStackTrace();
		}

		// Convert to a JsonObject
		JsonObject json = new JsonObject(sb.toString());
		System.out.println(json.encodePrettily());

		// Extract the JsonArray to build a new List of Zone objects
		JsonArray zoneArray = json.getJsonArray("zones");

		List<Zone> zonesList = new ArrayList<>();
		zoneArray.forEach(zone -> {
			Zone z = null;
			try {
				z = (Zone) zone;
			} catch (Exception e) {
				z = Json.decodeValue(((JsonObject) zone).encode(), Zone.class);
			} finally {
				// Create a new Zone each time to compute the UID
				zonesList.add(new Zone(z.getNum(), z.getRect()));
			}
		});

		// Create the Zones object, and convert it into a prettily encoded Json String
		Zones zones = new Zones(zonesList);
		JsonObject newJson = new JsonObject(Json.encode(zones));
		newJson.remove("empty");
		String newValues = newJson.encodePrettily();
		System.out.println(newValues);

		// Save the String into a new File
		try (BufferedWriter writer = Files.newBufferedWriter(Paths.get(newFilename), StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
			writer.write(newValues);
		} catch (Exception e) {
			e.printStackTrace();
		}

	}
}
