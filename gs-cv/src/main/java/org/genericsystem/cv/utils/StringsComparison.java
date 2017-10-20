package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * This class contains various methods that can be used to compare {@link String} objects.
 * 
 * @author Pierrik Lassalas
 */
public class StringsComparison {

	private static final int DEFAULT_K = 3;
	private static final Pattern SPACE_PATTERN = Pattern.compile("\\s+");

	/**
	 * Describe the method used to compute the similarity
	 * 
	 * @author Pierrik Lassalas
	 */
	public enum SIMILARITY {
		/**
		 * Use the normalized Levenshtein distance (similarity = 1 - distance)
		 */
		LEVENSHTEIN,
		/**
		 * Use letter-pairs similarity
		 */
		LETTER_PAIRS,
		/**
		 * Use the cosine similarity to compare individual chars
		 */
		COSINE_CHAR,
		/**
		 * Use the cosine similarity to compare individual words
		 */
		COSINE_WORD;
	}

	public static void main(String[] args) {
		// System.out.println(getShingles("Bonjour monsieur, comment allez-vous aujourd'hui ?", 3));
		// System.out.println(getShinglesSpacesRemoved("Bonjour monsieur, comment allez-vous aujourd'hui ?", 3));
		// System.out.println(containsSubstring("Bonjour mon ami, avez-vous vu le soleil ?", "am\ni "));
		// System.out.println(containsSubstring("Bonjour mon ami, avez-vous vu le soleil ?", "chien"));
		System.out.println(containsSubstring("Bonjour mon ami, avez-vous vu le soleil ?", "solell", 0.7, SIMILARITY.LEVENSHTEIN));
	}

	/**
	 * Check whether a candidate string is contained in the original string.
	 * 
	 * @param original - the original string
	 * @param candidate - the candidate, expected to be found in <code>original</code>
	 * @return <code>true</code> if a match was found, <code>false</code> otherwise
	 */
	public static boolean containsSubstring(String original, String candidate) {
		if (original == null || candidate == null)
			throw new IllegalArgumentException("Attempt to compare one (or more) null strings");
		if (original.trim().isEmpty() || candidate.trim().isEmpty())
			return false;
		String candidateNoSpaces = SPACE_PATTERN.matcher(candidate.toLowerCase()).replaceAll("");
		int k = candidateNoSpaces.length();
		Set<String> shingles = getShinglesSpacesRemoved(original.toLowerCase(), k);
		return shingles.stream().anyMatch(s -> s.equals(candidateNoSpaces));
	}

	/**
	 * Check whether a candidate string is contained in the original string.
	 * 
	 * @param original - the original string
	 * @param candidate - the candidate, expected to be found in <code>original</code>
	 * @param similarityThreshold - the similarity threshold
	 * @param option - the method to be used for string comparison
	 * @return <code>true</code> if a match was found, <code>false</code> otherwise
	 */
	public static boolean containsSubstring(String original, String candidate, double similarityThreshold, SIMILARITY option) {
		if (original == null || candidate == null)
			throw new IllegalArgumentException("Attempt to compare one (or more) null strings");
		if (original.trim().isEmpty() || candidate.trim().isEmpty())
			return false;
		String candidateNoSpaces = SPACE_PATTERN.matcher(candidate.toLowerCase()).replaceAll("");
		int k = candidateNoSpaces.length();
		Set<String> shingles = getShinglesSpacesRemoved(original.toLowerCase(), k);
		return shingles.stream().anyMatch(s -> {
			double sim = compare(s, candidateNoSpaces, option);
			return sim > similarityThreshold ? true : false;
		});
	}

	public static double compare(String string1, String string2, SIMILARITY option) {
		double sim = 0;
		switch (option) {
		default:
		case LEVENSHTEIN:
			sim = Levenshtein.similarity(string1, string2);
			break;
		case LETTER_PAIRS:
			sim = LetterPairSimilarity.compareStrings(string1, string2);
			break;
		case COSINE_CHAR:
			sim = CosineSimilarity.cosineSimilarity(string1, string2, CosineSimilarity.PATTERN.SINGLE_CHAR);
			break;
		case COSINE_WORD:
			sim = CosineSimilarity.cosineSimilarity(string1, string2, CosineSimilarity.PATTERN.WORDS);
			break;
		}
		return sim;
	}

	/**
	 * Decompose a string in shingles of default size, without considering words (i.e., all spaces are removed).
	 * 
	 * @param string - the input string
	 * @return a {@link Set} containing the individual shingles (lowercased)
	 */
	public static Set<String> getShinglesSpacesRemoved(final String string) {
		return getShinglesSpacesRemoved(string, DEFAULT_K);
	}

	/**
	 * Decompose a string in shingles of size <code>k</code>, without considering words (i.e., all spaces are removed).
	 * 
	 * @param string - the input string
	 * @param k - the size of the shingles
	 * @return a {@link Set} containing the individual shingles (lowercased)
	 */
	public static Set<String> getShinglesSpacesRemoved(final String string, int k) {
		String stringNoSpaces = SPACE_PATTERN.matcher(string).replaceAll("");
		return getShingles(stringNoSpaces, k);
	}

	/**
	 * Decompose a string in shingles of default size.
	 * 
	 * @param string - the input string
	 * @return a {@link Set} containing the individual shingles (lowercased)
	 */
	public static Set<String> getShingles(final String string) {
		return getShingles(string, DEFAULT_K);
	}

	/**
	 * Decompose a string in small chunks (shingles) of size <code>k</code>.
	 * 
	 * @param string - the input string
	 * @param k - the size of the shingles
	 * @return a {@link Set} containing the individual shingles (lowercased)
	 */
	public static Set<String> getShingles(final String string, int k) {
		if (k <= 0)
			throw new IllegalArgumentException(String.format("k should be positive (provided value: %d)", k));
		if (string == null || string.isEmpty())
			return Collections.emptySet();
		Set<String> shingles = SPACE_PATTERN.splitAsStream(string.toLowerCase()).flatMap(word -> {
			if (word.length() < k)
				return Stream.empty();
			int size = word.length() - k + 1;
			List<String> tmp = new ArrayList<>(size);
			for (int i = 0; i < size; ++i) {
				tmp.add(word.substring(i, i + k));
			}
			return tmp.stream();
		}).collect(Collectors.toSet());
		return shingles;
	}

}
