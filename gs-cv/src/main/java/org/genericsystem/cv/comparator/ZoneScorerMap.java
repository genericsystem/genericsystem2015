package org.genericsystem.cv.comparator;

import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Scores;
import org.genericsystem.cv.Zone;

public class ZoneScorerMap {

	private FileWriter writer;
	private static final String basePath = "classes/id-fr-front/csv/";
	private static final String delimiter = "\t";

	private final Scores scores = new Scores();
	private final Zone zone;

	public ZoneScorerMap(Zone zone, Stream<Entry<Img, String>> stream, String filename) {
		this.zone = zone;

		try {
			// Open a file to log the data (default: append = true)
			writer = new FileWriter(basePath + filename.replaceAll(".png", "") + ".csv", true);
			// Log the zone number
			log(writer, "Zone", Integer.toString(zone.getNum()));
			
			// Loop over each entry and get the OCR
			stream.forEach(entry -> {
				String ocrText = zone.ocr(entry.getKey());
				ocrText = ocrText.replace("\n", "").replaceAll("\t", "").trim();
				scores.put(ocrText);
				scores.put(entry.getValue(), ocrText);
			});
			// Call the garbage collector to free the resources
			System.gc();
			
			// Log every OCR and filter names
			log(writer, this.getResultsMap());
			log(writer, "bestLevenshtein", this.getMinLevenshtein());
			log(writer, "bestText", this.getBestText());
			log(writer, "bestText2", this.getBestText2());
			// Close the file
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void log(FileWriter writer, String... strings) throws IOException {
		for (String s : strings) {
			writer.append(s).append(delimiter);
		}
		writer.append("\n");
	}
	
	private void log(FileWriter writer, Map<String, Integer> map) throws IOException {
		StringBuffer line1 = new StringBuffer("Filtername").append(delimiter);
		StringBuffer line2 = new StringBuffer("Levenshtein").append(delimiter);
		map.entrySet().stream().forEach(entry -> {
			line1.append(entry.getKey()).append(delimiter);
			line2.append(entry.getValue().toString()).append(delimiter);
		});
		writer.append(line1.toString()).append("\n");
		writer.append(line2.toString()).append("\n");
	}

	public Zone getZone() {
		return zone;
	}

	public double getBestScore() {
		return scores.getBestScore();
	}
	
	public Map<String, Integer> getResultsMap(){
		return scores.getResultsMap();
	}
	
	public String getMinLevenshtein(){
		return scores.getMinLevenshtein().toString();
	}

	public String getBestText() {
		return scores.getBestText();
	}

	public String getBestText2() {
		return scores.getBestText2();
	}

}
