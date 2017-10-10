package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.layout.Ransac;
import org.genericsystem.layout.Ransac.Model;

public class OCRPlasty {

	public static enum RANSAC {
		NONE,
		LCS,
		SIMILARITY,
		LEVENSHTEIN
	}

	public static void main(String[] args) {
		List<String> labels = new ArrayList<>();
		labels.add("had I expressed the agony I frequentl felt he would have been taught to long for its alleviati");
		labels.add("gad I sed the agony I fefjuently felt he would have been to long for its alleviafcion");
		labels.add("had I expressed tbe agony I frejuently felt he would have been taught to long for its alleviationq");
		labels.add("had I expresset th agny I frequently feltu he wouald have ben taufht to lng fr its alevation");
		labels.add("had I # tly feltu he wouald have ben taufht to lng fr iets alevation");
		labels.add("fger gezrgze ertg");

		System.out.println(ocrPlasty(getRansacInliers(new ArrayList<>(labels), getModelProviderMaxLcs(getMaxLcsLength(labels)))));
	}

	public static String correctStrings(List<String> labels, OCRPlasty.RANSAC options) {
		List<String> trimmed = labels.stream().map(s -> s.trim()).filter(s -> s.length() > 0).collect(Collectors.toList());
		Function<Collection<String>, Model<String>> modelProvider = null;
		switch (options) {
		default:
		case NONE:
			return ocrPlasty(trimmed);
		case LCS:
			modelProvider = getModelProviderMaxLcs(getMaxLcsLength(trimmed));
			break;
		case SIMILARITY:
			modelProvider = getModelProviderSimilarity(getMaxSimilarity(trimmed));
			break;
		case LEVENSHTEIN:
			modelProvider = getModelProviderLevenshtein(getMaxLevenshtein(trimmed));
			break;
		}
		List<String> inliers = getRansacInliers(trimmed, modelProvider);
		return ocrPlasty(inliers);
	}

	public static double similarity(List<String> strings) {
		double sim = 0;
		int n = strings.size();
		if (n == 1)
			return 1;
		for (int i = 0; i < n; i++) {
			for (int j = i + 1; j < n; j++) {
				// each distance will be between 0 and 1
				sim += Levenshtein.distance(strings.get(i), strings.get(j)) / ((double) strings.get(i).length() + strings.get(j).length());
			}
		}
		return 1 - 2 * sim / n / (n - 1); // divide by the total number of distances
	}

	private static String ocrPlasty(List<String> labels) {
		if (labels == null || labels.isEmpty())
			throw new IllegalStateException("Attempt to compute the longestCommonSubsequence on an empty list");
		String common = longestCommonSubsequence(labels);
		String consensus = "";
		for (int i = 0; i < common.length() + 1; i++) {
			List<String> candidates = new ArrayList<>();
			for (int label = 0; label < labels.size(); label++) {
				List<String> is = (i < common.length()) ? interString(labels.get(label), common.charAt(i)) : endString(labels.get(label));
				labels.set(label, is.get(0));
				candidates.add(is.get(1));
			}
			consensus += selectBest(candidates);
			if (i < common.length() - 1)
				consensus += common.charAt(i);
		}
		return consensus;
	}

	private static List<String> getRansacInliers(List<String> labels, Function<Collection<String>, Model<String>> modelProvider) {
		List<String> trimmed = labels.stream().map(s -> s.trim()).filter(s -> s.length() > 0).collect(Collectors.toList());
		if (trimmed.isEmpty())
			return Collections.emptyList();

		Map<Integer, String> bestFit = new HashMap<>();
		int t = 1;
		for (int i = 1, maxAttempts = 10; bestFit.size() <= 3 && i <= maxAttempts; ++i) {
			Ransac<String> ransac = new Ransac<>(trimmed, modelProvider, 3, 50 * i, t, trimmed.size() / 2);
			try {
				ransac.compute();
				bestFit = ransac.getBestDataSet();
				bestFit.entrySet().forEach(entry -> System.out.println("key: " + entry.getKey() + " | value: " + entry.getValue()));
			} catch (Exception e) {
				t += 1;
				System.err.println("Can't get a good model. Increase the error margin to " + t);
			}
		}
		return bestFit.values().stream().collect(Collectors.toList());
	}

	private static int getMaxLcsLength(List<String> labels) {
		return labels.stream().map(s -> s.length()).max((x, y) -> Integer.compare(x, y)).orElse(0);
	}

	private static double getMaxSimilarity(List<String> labels) {
		double max = 0d;
		for (int i = 0; i < labels.size(); ++i) {
			String base = labels.get(i);
			for (int j = 0; j < labels.size(); ++j) { // could use int j = i, but not possible with an iterator in the modelprovider
				max += LetterPairSimilarity.compareStrings(base, labels.get(j));
			}
		}
		return max;
	}

	private static double getMaxLevenshtein(List<String> labels) {
		double max = 0d;
		for (int i = 0; i < labels.size(); ++i) {
			String base = labels.get(i);
			for (int j = 0; j < labels.size(); ++j) { // could use int j = i, but not possible with an iterator in the modelprovider
				max += Levenshtein.distance(base, labels.get(j)) / ((double) base.length() + labels.get(j).length());
			}
		}
		return max;
	}

	private static Function<Collection<String>, Model<String>> getModelProviderMaxLcs(int maxLength) {
		return datas -> {
			Iterator<String> it = datas.iterator();
			String subsequence = null;
			if (it.hasNext())
				subsequence = it.next();
			while (it.hasNext()) {
				String label = it.next().trim();
				if (!(subsequence.isEmpty() || label.isEmpty()))
					subsequence = lcs(subsequence, label);
			}
			String common = subsequence;

			return new Model<String>() {
				@Override
				public double computeError(String data) {
					String lcs = lcs(data, common);
					return Math.abs(lcs.length() - maxLength); // common.length() - maxLength
				}

				@Override
				public Object[] getParams() {
					return new Object[] { common };
				}
			};
		};
	}

	private static Function<Collection<String>, Model<String>> getModelProviderSimilarity(double maxSimilarity) {
		return datas -> {
			return new Model<String>() {
				private double max = 0d;

				@Override
				public double computeError(String data) {
					for (String s : datas) {
						if (s != data) {
							max += LetterPairSimilarity.compareStrings(data, s);
						}
					}
					return Math.abs(max - maxSimilarity);
				}

				@Override
				public Object[] getParams() {
					return new Object[] { max };
				}
			};
		};
	}

	private static Function<Collection<String>, Model<String>> getModelProviderLevenshtein(double maxLevenshtein) {
		return datas -> {
			return new Model<String>() {
				private double max = 0d;

				@Override
				public double computeError(String data) {
					for (String s : datas) {
						if (s != data) {
							max += Levenshtein.distance(data, s);
						}
					}
					return Math.abs(max - maxLevenshtein);
				}

				@Override
				public Object[] getParams() {
					return new Object[] { max };
				}
			};
		};
	}

	private static String selectBest(List<String> candidates) {
		Map<String, Integer> occurrences = new HashMap<>();
		for (String s : candidates)
			occurrences.put("@" + s, (occurrences.containsKey("@" + s)) ? occurrences.get("@" + s) + 1 : 1);
		int maxOcc = Collections.max(occurrences.values());
		if (maxOcc > 1) {
			for (Map.Entry<String, Integer> e : occurrences.entrySet()) {
				if (e.getValue().equals(maxOcc))
					return e.getKey().substring(1);
			}
		}
		return leastDifferent(candidates); // if there's no candidate with at least 2 occurrences
	}

	private static List<String> interString(String string, char c) { // string between 2 consecutive elements of the lcs
		String inter = "";
		int index = string.indexOf(c);
		if (index > 0)
			inter = string.substring(0, index);
		string = string.substring(index + 1);
		List<String> is = new ArrayList<>();
		is.add(string);
		is.add(inter);
		return is;
	}

	private static List<String> endString(String string) { // string following the last element of the lcs
		List<String> is = new ArrayList<>();
		is.add("");
		is.add(string);
		return is;
	}

	private static String leastDifferent(List<String> strings) { // "least different" string from the others (smallest sum of Levenshtein distance with the others)
		int n = strings.size();
		int[][] distances = new int[n][n];
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (j > i) {
					int dist = Levenshtein.distance(strings.get(i), strings.get(j));
					distances[i][j] = dist;
					distances[j][i] = dist;
				}
			}
			distances[i][i] = 0;
		}
		int minVal = Integer.MAX_VALUE;
		String leastDiff = "";
		for (int i = 0; i < n; i++) {
			int val = IntStream.of(distances[i]).sum();
			if (val < minVal) {
				leastDiff = strings.get(i);
				minVal = val;
			}
		}
		return leastDiff;
	}

	private static String longestCommonSubsequence(List<String> labels) { // lcs between n strings
		String subsequence = labels.get(0).trim();
		for (int i = 1; i < labels.size(); i++) {
			if (!(subsequence.isEmpty() || labels.get(i).trim().isEmpty()))
				subsequence = lcs(subsequence, labels.get(i).trim());
		}
		return subsequence;
	}

	private static String lcs(String stringX, String stringY) { // lcs between 2 strings
		int m = stringX.length();
		int n = stringY.length();
		int[][] mat = new int[m + 1][n + 1];
		// Following steps build mat[m+1][n+1] in bottom up fashion. Note that mat[i][j] contains length of LCS of X[0..i-1] and Y[0..j-1]
		for (int i = 0; i <= m; i++) {
			for (int j = 0; j <= n; j++) {
				if (i == 0 || j == 0)
					mat[i][j] = 0;
				else if (stringX.charAt(i - 1) == stringY.charAt(j - 1))
					mat[i][j] = mat[i - 1][j - 1] + 1;
				else
					mat[i][j] = Math.max(mat[i - 1][j], mat[i][j - 1]);
			}
		}
		// Following code is used to print LCS
		int index = mat[m][n];
		int temp = index;
		// Create a character array to store the lcs string
		char[] lcs = new char[index + 1];
		lcs[index] = '\0'; // Set the terminating character
		// Start from the right-most-bottom-most corner and one by one store characters in lcs[]
		int i = m, j = n;
		while (i > 0 && j > 0) {
			// If current character in X[] and Y are same, then current character is part of LCS
			if (stringX.charAt(i - 1) == stringY.charAt(j - 1)) {
				// Put current character in result
				lcs[index - 1] = stringX.charAt(i - 1);
				// reduce values of i, j and index
				i--;
				j--;
				index--;
			}
			// If not same, then find the larger of two and go in the direction of larger value
			else if (mat[i - 1][j] > mat[i][j - 1])
				i--;
			else
				j--;
		}
		String lcsValue = "";
		for (int k = 0; k <= temp; k++)
			lcsValue += lcs[k];

		return lcsValue;
	}

}
