package org.genericsystem.cv.comparator;

import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Stream;

import org.genericsystem.cv.Img;
import org.genericsystem.cv.Scores;
import org.genericsystem.cv.Zone;

public class ZoneScorerMap2 {

	private boolean supervised;
	private FileWriter writer;
	private static final String basePath = "classes/id-fr-front/csv/";
	private static final String delimiter = "\t";

	private final Scores scores = new Scores();
	private final Zone zone;

	public ZoneScorerMap2(Zone zone, Stream<Entry<Img, String>> stream, String filename, boolean supervised) {
		this.zone = zone;
		this.supervised = supervised;
		if (supervised){
			computeScorer(stream, filename);
		} else {
			computeScorer(stream, filename);
		}	
	}

	private void computeScorer(Stream<Entry<Img, String>> stream, String filename) {
		try {
			// Open a file to log the data (default: append = true)
			writer = new FileWriter(basePath + filename.replaceAll(".png", "") + ".csv", true);
			
			// Store the filename in the scores
			scores.setFilename(filename);
			scores.setZone(zone.getNum());
			
			// Loop over each entry and get the OCR
			stream.forEach(entry -> {
				String ocrText = zone.ocr(entry.getKey());
				ocrText = ocrText.replace("\n", "").replaceAll("\t", "").trim();
				// Store the OCR text
				scores.put(ocrText);
				// Store the OCR text corresponding to the filter
				scores.put(entry.getValue(), ocrText);
			});
			// Call the garbage collector to free the resources
			System.gc();

			// Log every OCR and filter names
			if (supervised){
				log(writer, this.getSupervisedResultsMap());
			} else {
				log(writer, this.getResultsMap());
			}
			
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
		map.entrySet().stream().forEach(entry -> {
			try {
				writer.append(Integer.toString(zone.getNum())).append(delimiter).append(entry.getKey())
						.append(delimiter).append(entry.getValue().toString()).append("\n");
			} catch (IOException e) {
				e.printStackTrace();
			}
		});
	}

	public Zone getZone() {
		return zone;
	}

	public double getBestScore() {
		return scores.getBestScore();
	}

	public Map<String, Integer> getResultsMap() {
		return scores.getResultsMap();
	}
	
	public Map<String, Integer> getSupervisedResultsMap() {
		return scores.getSupervisedResultsMap();
	}

	public String getMinLevenshtein() {
		return scores.getMinLevenshtein().toString();
	}

	public String getBestText() {
		return scores.getBestText();
	}

	public String getBestText2() {
		return scores.getBestText2();
	}

}
