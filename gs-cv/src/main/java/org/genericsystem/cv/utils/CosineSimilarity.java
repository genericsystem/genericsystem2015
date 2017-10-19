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

	private static final Pattern SINGLE_CHAR_PATTERN = Pattern.compile("(?!^)");

	public static void main(String[] args) {
		System.out.println(cosineSimilarity("bob", "rob"));
		System.out.println(cosineSimilarity("hello", "molehill"));
	}

	/**
	 * Compute the cosine similarity between two strings.
	 * 
	 * @param string1 - the first string
	 * @param string2 - the second string
	 * @return the cosine similarity between two strings
	 */
	public static double cosineSimilarity(String string1, String string2) {
		if (null == string1 || null == string2)
			throw new IllegalArgumentException("Cosine similarity requires two not null strings");
		if (string1.equals(string2))
			return 1.0;
		if (string1.isEmpty() || string2.isEmpty())
			return 0;

		// Get the frequency maps
		Map<String, Long> a = getFrequencyMap(string1);
		Map<String, Long> b = getFrequencyMap(string2);

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
	 * @return a {@link Map} containing the individual characters and their frequencies
	 */
	public static Map<String, Long> getFrequencyMap(String string) {
		List<String> chars = Arrays.asList(SINGLE_CHAR_PATTERN.split(string.trim()));
		return chars.stream().collect(Collectors.groupingBy(s -> s, Collectors.counting()));
	}
}
