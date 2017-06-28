package org.genericsystem.cv;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

//TODO compute score / bestText on every add ?
public class Scores {

	private final Map<String, Integer> ocrs = new HashMap<>();
	private final List<String> ocrs2 = new ArrayList<>();

	private final Map<String, String> ocrResults = new HashMap<>();
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
	 *         total Levenshtein distance as the value
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

	/**
	 * Compute the supervised scores for a given OCR result.
	 * 
	 * The Map {@link #ocrResults}, which contains the filtername as a key and
	 * the OCR text as the value, is analyzed. A new map is created, containing
	 * the filtername as key and the Levenshtein distance as value.
	 * 
	 * @return A Map containing the filtername as the key, and the corresponding
	 *         Levenshtein distance (compared to the real value) as the value
	 */
	public Map<String, Integer> getSupervisedResultsMap() {
		Map<String, Integer> results = new HashMap<>();
		// If the text value is readable, compare with each OCR text and store
		// the Levenshtein distance. For convenience, all spaces are removed
		if (this.realText != null && !this.realText.isEmpty()) {
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
		int bestScore = 0;
		int allOcrs = 0;
		for (Entry<String, Integer> entry : ocrs.entrySet()) {
			allOcrs += entry.getValue();
			if (bestScore < entry.getValue()) {
				bestScore = entry.getValue();
			}
		}
		return Integer.valueOf(bestScore).doubleValue() / allOcrs;
	}

	// Best text = most occurrence
	public String getBestText() {
		String bestText = "";
		int bestScore = 0;
		for (Entry<String, Integer> entry : ocrs.entrySet()) {
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

	public String getRealText() {
		return realText;
	}

	public void setRealText(String realText) {
		this.realText = realText;
	}
}
