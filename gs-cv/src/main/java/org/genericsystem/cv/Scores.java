package org.genericsystem.cv;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.cv.comparator.ZoneRealValue;
import org.genericsystem.cv.model.Doc;
import org.genericsystem.cv.model.DocClass;
import org.genericsystem.cv.model.ImgFilter;
import org.genericsystem.cv.model.ZoneGeneric;
import org.genericsystem.cv.model.ZoneText;
import org.genericsystem.cv.model.Doc.DocInstance;
import org.genericsystem.cv.model.DocClass.DocClassInstance;
import org.genericsystem.cv.model.ZoneText.ZoneTextInstance;
import org.genericsystem.kernel.Engine;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

//TODO compute score / bestText on every add ?
public class Scores {

	private static final ObjectMapper mapper = new ObjectMapper();
	private static final String basePath = "classes/id-fr-front";

	private final Map<String, Integer> ocrs = new HashMap<>();
	private final List<String> ocrs2 = new ArrayList<>();

	private final Map<String, String> ocrResults = new HashMap<>();
	private String filename;
	private int zone;
	private Integer minLevenshtein;
	private String realText;

	public void put(String s) {
		Integer count = ocrs.get(s);
		ocrs.put(s, 1 + (count != null ? count : 0));
		ocrs2.add(s);
	}

	public void put(String filtername, String ocr) {
		ocrResults.put(filtername, ocr);
	}

	/**
	 * Compute the Levenshtein distance of all OCR text.
	 * 
	 * The Map {@link #ocrResults}, which contains the filtername as a key and
	 * the OCR text as the value, is analyzed. The {@link #minLevenshtein} value
	 * is also set with the minimum distance found.
	 * 
	 * @return A Map containing the filtername as the key, and the corresponding
	 *         Levenshtein distance as the value
	 */
	public Map<String, Integer> getResultsMap() {
		Map<String, Integer> results = new HashMap<>();
		int shorterDistance = Integer.MAX_VALUE;
		for (Entry<String, String> entry : ocrResults.entrySet()) {
			int dist = 0;
			for (Entry<String, String> entry2 : ocrResults.entrySet()) {
				dist += Levenshtein.distance(entry.getValue(), entry2.getValue());
			}
			results.put(entry.getKey(), dist);
			if (dist < shorterDistance) {
				shorterDistance = dist;
			}
		}
		minLevenshtein = shorterDistance;
		return results;
	}

	public Map<String, Integer> getSupervisedResultsMap() {
		Map<String, Integer> results = new HashMap<>();
		
		/*
		 * If the text value is readble, compare with each OCR text and store the Levenshtein distance.
		 * For convenience, all spaces are removed
		 */
		if (this.realText != null && !this.realText.isEmpty() ) {
			System.out.println("Trained data found! Using supervised training");
			for (Entry<String, String> entry : ocrResults.entrySet()) {
				int dist = Levenshtein.distance(entry.getValue().replaceAll("[ .,]", "").trim(),
						this.realText.replaceAll("[ .,]", "").trim());
				results.put(entry.getKey(), dist);
			}
		} else {
			System.out.println("Unable to find the zone! Using unsupervised training");
			return getResultsMap();
		}
		return results;
	}

	public Integer getMinLevenshtein() {
		return minLevenshtein;
	}

	public double getBestScore() {
		String bestText = "";
		int bestScore = 0;
		int allOcrs = 0;
		for (Entry<String, Integer> entry : ocrs.entrySet()) {
			allOcrs += entry.getValue();
			if (bestScore < entry.getValue()) {
				bestScore = entry.getValue();
				bestText = entry.getKey();
			}
		}
		return Integer.valueOf(bestScore).doubleValue() / allOcrs;
	}

	// Best text = most occurrence
	public String getBestText() {
		String bestText = "";
		int bestScore = 0;
		int allOcrs = 0;
		for (Entry<String, Integer> entry : ocrs.entrySet()) {
			allOcrs += entry.getValue();
			if (bestScore < entry.getValue()) {
				bestScore = entry.getValue();
				bestText = entry.getKey();
			}
		}
		return bestText;
	}

	// Best text = shorter Levenshtein distance
	public String getBestText2() {
		String bestText = "";
		int shorterDistance = Integer.MAX_VALUE;
		for (String key : ocrs2) {
			if (!"".equals(key)) {
				int d = 0;
				for (String key2 : ocrs2) {
					// System.out.println("key2 : " + key2 + " Levenshtein : " +
					// Levenshtein.distance(key, key2));
					d += Levenshtein.distance(key, key2);
				}
				// System.out.println("key : " + key + " somme distance : " +
				// d);
				if (d < shorterDistance) {
					bestText = key;
					shorterDistance = d;
				}
			}
		}
		// System.out.println("best text: " + bestText);
		// System.out.println("shorter distance: " + shorterDistance);
		return bestText;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public void setZone(int zone) {
		this.zone = zone;
	}

	public String getRealText() {
		return realText;
	}

	public void setRealText(String realText) {
		this.realText = realText;
	}
}
