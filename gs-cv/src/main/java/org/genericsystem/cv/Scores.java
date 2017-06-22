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

	public void put(String s) {
		Integer count = ocrs.get(s);
		ocrs.put(s, 1 + (count != null ? count : 0));
		ocrs2.add(s);
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
		for (String key : ocrs2) {
			if (!"".equals(key)) {
				int d = 0;
				for (String key2 : ocrs2) {
//					 System.out.println("key2 : " + key2 + " Levenshtein : " + Levenshtein.distance(key, key2));
					d += Levenshtein.distance(key, key2);
				}
//				 System.out.println("key : " + key + " somme distance : " + d);
				if (d < shorterDistance) {
					bestText = key;
					shorterDistance = d;
				}
			}
		}
//		System.out.println("best text: " + bestText);
//		System.out.println("shorter distance: " + shorterDistance);
		return bestText;
	}
}
