package org.genericsystem.cv;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

//TODO compute score / bestText on every add ?
public class Scores {

	private final Map<String, Integer> ocrs = new HashMap<>();

	public void put(String s) {
		Integer count = ocrs.get(s);
		ocrs.put(s, 1 + (count != null ? count : 0));
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

	public String getBestText2() {
		String bestText = "";
		int shorterDistance = Integer.MAX_VALUE;
		for (Entry<String, Integer> entry : ocrs.entrySet()) {
			if (!entry.getKey().equals("")) {
				int d = 0;
				for (Entry<String, Integer> entry2 : ocrs.entrySet()) {
					d += Levenshtein.distance(entry.getKey(), entry2.getKey());
				}
				if (d < shorterDistance) {
					bestText = entry.getKey();
					shorterDistance = d;
				}
			}
		}
		return bestText;
	}
}
