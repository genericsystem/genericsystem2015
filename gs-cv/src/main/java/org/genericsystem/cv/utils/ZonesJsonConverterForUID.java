package org.genericsystem.cv.utils;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.cv.Zone;
import org.genericsystem.cv.Zones;

import io.vertx.core.json.Json;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class ZonesJsonConverterForUID {

	private static final String filename = "/zones/zones-old.json";
	private static final String newFilename = "/zones/zones.json";

	public static void main(String[] args) {
		List<String> paths = new ArrayList<>();
		for (String docClass : Arrays.asList("id-fr-front", "passport-fr")) {
			paths.add(System.getenv("HOME") + "/genericsystem/gs-ir-files/classes/" + docClass + "/");
			paths.add(System.getProperty("user.dir") + "/../gs-cv/classes/" + docClass + "/");
		}
		System.out.println(paths);
		updateJson(paths);
	}

	public static void updateJson(List<String> paths) {
		for (String basePath : paths) {
			Path pathname = Paths.get(basePath + filename);
			Path newPathname = Paths.get(basePath + newFilename);

			// If zones-old.json doesn't exist:
			if (!pathname.toFile().exists()) {
				// If zones.json exists:
				if (newPathname.toFile().exists()) {
					// Copy zones -> zones-old; then remove zones
					try {
						Files.copy(newPathname, pathname, StandardCopyOption.REPLACE_EXISTING);
						newPathname.toFile().delete();
					} catch (IOException e) {
						throw new IllegalStateException(e);
					}
				} else {
					throw new IllegalStateException(newPathname + " not found");
				}
			} else {
				System.out.println("Found " + pathname);
			}

			// Read the initial file into a StringBuffer
			StringBuffer sb = new StringBuffer();
			try {
				for (String string : Files.readAllLines(pathname)) {
					sb.append(string);
				}
			} catch (IOException e) {
				throw new IllegalStateException(e);
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
			try (BufferedWriter writer = Files.newBufferedWriter(newPathname, StandardCharsets.UTF_8, StandardOpenOption.CREATE_NEW)) {
				writer.write(newValues);
			} catch (FileAlreadyExistsException e) {
				System.out.println(newPathname + " already exists. Skipped...");
			} catch (Exception e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
