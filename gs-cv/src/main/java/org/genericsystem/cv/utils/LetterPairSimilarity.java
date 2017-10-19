package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class LetterPairSimilarity {

	private static final Pattern SPACE_PATTERN = Pattern.compile("\\s+");

	public static void main(String[] args) {
		String string = "healed";
		String[] strings = new String[] { "healed", "sealed", "healthy", "heard", "herded", "help", "sold" };
		for (String s : strings) {
			double compare = LetterPairSimilarity.compareStrings(string, s);
			System.out.println(String.format("Similarity = %.3f for %s and %s", compare, string, s));
		}
	}

	public static double compareStrings(String string1, String string2) {
		if (null == string1 || null == string2)
			throw new IllegalArgumentException("LetterPairSimilarity requires two not null strings");
		if (string1.equals(string2))
			return 1d;
		if (string1.isEmpty())
			return 0;
		if (string2.isEmpty())
			return 0;

		List<String> pairs1 = wordLetterPairs(string1.toLowerCase());
		List<String> pairs2 = wordLetterPairs(string2.toLowerCase());
		int intersection = 0;
		int union = pairs1.size() + pairs2.size();
		for (int i = 0; i < pairs1.size(); ++i) {
			String pair1 = pairs1.get(i);
			for (int j = 0; j < pairs2.size(); ++j) {
				String pair2 = pairs2.get(j);
				if (pair1.equals(pair2)) {
					intersection++;
					pairs2.remove(j);
					break;
				}
			}
		}
		return (2d * intersection) / union;
	}

	private static List<String> wordLetterPairs(String string) {
		List<String> allPairs = new ArrayList<>();
		String[] words = SPACE_PATTERN.split(string);
		for (int i = 0; i < words.length; ++i) {
			String[] pairsInWord = letterPairs(words[i]);
			for (int j = 0; j < pairsInWord.length; ++j) {
				allPairs.add(pairsInWord[j]);
			}
		}
		return allPairs;
	}

	private static String[] letterPairs(String string) {
		if (string.length() < 1)
			return new String[] {};
		int numPairs = string.length() - 1;
		String[] pairs = new String[numPairs];
		for (int i = 0; i < numPairs; ++i) {
			pairs[i] = string.substring(i, i + 2);
		}
		return pairs;
	}
}
