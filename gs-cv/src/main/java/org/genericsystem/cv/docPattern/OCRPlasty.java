package org.genericsystem.cv.docPattern;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.IntStream;

import org.genericsystem.cv.Levenshtein;

public class OCRPlasty {

	public static void main(String[] args) {
		List<String> labels = new ArrayList<>();
		labels.add("had I expressed the agony I frequentl felt he would have been taught to long for its alleviati");
		labels.add("gad I sed the agony I fefjuently felt he would have been to long for its alleviafcion");
		labels.add("had I expressed tbe agony I frejuently felt he would have been taught to long for its alleviationq");
		labels.add("had I expresset th agny I frequently feltu he wouald have ben taufht to lng fr its alevation");

		System.out.println(similarity(labels));

		System.out.println(ocrPlasty(labels));

	}

	public static String ocrPlasty(List<String> labels) {
		if (labels == null || labels.isEmpty())
			throw new IllegalStateException("Attempted to compute the longestCommonSubsequence on an empty list");
		String common = longestCommonSubsequence(labels);
		String consensus = "";
		for (int i = 0; i < common.length() + 1; i++) {
			List<String> candidates = new ArrayList<>();
			for (int l = 0; l < labels.size(); l++) {
				List<String> is = (i < common.length()) ? interString(labels.get(l), common.charAt(i)) : endString(labels.get(l));
				labels.set(l, is.get(0));
				candidates.add(is.get(1));
			}
			consensus += selectBest(candidates);
			if (i < common.length() - 1)
				consensus += common.charAt(i);
		}
		return consensus;
	}

	public static String selectBest(List<String> candidates) {
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

	private static String leastDifferent(List<String> strings) { // "least different" string from the others (smallest
																	// sum of levenshtein distance with the others)
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

	public static String longestCommonSubsequence(List<String> labels) { // lcs between n strings
		String subsequence = labels.get(0).trim();
		for (int i = 1; i < labels.size(); i++) {
			if (!(subsequence.isEmpty() || labels.get(i).trim().isEmpty()))
				subsequence = lcs(subsequence, labels.get(i).trim());
		}
		return subsequence;
	}

	public static double similarity(List<String> strings) {
		double sim = 0;
		int n = strings.size();
		if (n == 1)
			return 1;
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				if (j > i) // each distance will be between 0 and 1
					sim += Levenshtein.distance(strings.get(i), strings.get(j)) / ((double) strings.get(i).length() + strings.get(j).length());
			}
		}
		return 1 - 2 * sim / n / (n - 1); // divide by the total number of distances
	}

	public static String lcs(String X, String Y) { // lcs between 2 strings

		int m = X.length();
		int n = Y.length();
		int[][] L = new int[m + 1][n + 1];
		// Following steps build L[m+1][n+1] in bottom up fashion. Note
		// that L[i][j] contains length of LCS of X[0..i-1] and Y[0..j-1]
		for (int i = 0; i <= m; i++) {
			for (int j = 0; j <= n; j++) {
				if (i == 0 || j == 0)
					L[i][j] = 0;
				else if (X.charAt(i - 1) == Y.charAt(j - 1))
					L[i][j] = L[i - 1][j - 1] + 1;
				else
					L[i][j] = Math.max(L[i - 1][j], L[i][j - 1]);
			}
		}
		// Following code is used to print LCS
		int index = L[m][n];
		int temp = index;
		// Create a character array to store the lcs string
		char[] lcs = new char[index + 1];
		lcs[index] = '\0'; // Set the terminating character
		// Start from the right-most-bottom-most corner and one by one store characters in lcs[]
		int i = m, j = n;
		while (i > 0 && j > 0) {
			// If current character in X[] and Y are same, then current character is part of LCS
			if (X.charAt(i - 1) == Y.charAt(j - 1)) {
				// Put current character in result
				lcs[index - 1] = X.charAt(i - 1);
				// reduce values of i, j and index
				i--;
				j--;
				index--;
			}
			// If not same, then find the larger of two and go in the direction of larger value
			else if (L[i - 1][j] > L[i][j - 1])
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
