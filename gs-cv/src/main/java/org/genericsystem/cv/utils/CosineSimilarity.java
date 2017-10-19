package org.genericsystem.cv.utils;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * Compute the cosine similarity between two strings.
 * 
 * @author Pierrik Lassalas
 */
public class CosineSimilarity {

	public static void main(String[] args) {
		System.out.println(cosineSimilarity("bob", "rob", PATTERN.SINGLE_CHAR));
		System.out.println(cosineSimilarity("hello", "molehill", PATTERN.SINGLE_CHAR));
	}

	/**
	 * Define the {@link Pattern} used to split the string when computing the cosine similarity.
	 * 
	 * @author Pierrik Lassalas
	 */
	public enum PATTERN {
		SINGLE_CHAR(Pattern.compile("(?!^)")),
		WORDS(Pattern.compile("\\W+")),
		SPACE(Pattern.compile("\\S+"));

		private Pattern pattern;

		PATTERN(Pattern splitPattern) {
			this.pattern = splitPattern;
		}

		public Pattern getPattern() {
			return pattern;
		}
	}

	/**
	 * Compute the cosine similarity between two strings.
	 * 
	 * @param string1 - the first string
	 * @param string2 - the second string
	 * @param splitPattern - the regex pattern that will be used to split the strings
	 * @return the cosine similarity between two strings
	 */
	public static double cosineSimilarity(String string1, String string2, PATTERN splitPattern) {
		if (null == string1 || null == string2)
			throw new IllegalArgumentException("Cosine similarity requires two not null strings");
		if (string1.equals(string2))
			return 1.0;
		if (string1.isEmpty() || string2.isEmpty())
			return 0;

		// Get the frequency maps
		Map<String, Long> a = getFrequencyMap(string1, splitPattern);
		Map<String, Long> b = getFrequencyMap(string2, splitPattern);

		// Compute the intersection
		Set<String> intersection = new HashSet<>(a.keySet());
		intersection.retainAll(b.keySet());

		// Compute the dot product and the magnitudes
		double dotProduct = intersection.stream().mapToDouble(s -> a.get(s) * b.get(s)).sum();
		double magnitudeA = a.values().stream().mapToDouble(i -> Math.pow(i, 2)).sum();
		double magnitudeB = b.values().stream().mapToDouble(i -> Math.pow(i, 2)).sum();

		return dotProduct / Math.sqrt(magnitudeA * magnitudeB);
	}

	/**
	 * Compute the frequency map of characters in a given string.
	 * 
	 * @param string - the input string
	 * @param splitPattern - the regex pattern that will be used to split the strings
	 * @return a {@link Map} containing the individual characters and their frequencies
	 */
	public static Map<String, Long> getFrequencyMap(String string, PATTERN splitPattern) {
		List<String> chars = Arrays.asList(splitPattern.getPattern().split(string.trim()));
		return chars.stream().collect(Collectors.groupingBy(s -> s, Collectors.counting()));
	}
}
