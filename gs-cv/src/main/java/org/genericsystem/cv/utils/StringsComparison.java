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

	public static Set<String> getShinglesSpacesRemoved(String string) {
		return getShinglesSpacesRemoved(string, DEFAULT_K);
	}

	public static Set<String> getShinglesSpacesRemoved(String string, int k) {
		String stringNoSpaces = SPACE_PATTERN.matcher(string).replaceAll("");
		return getShingles(stringNoSpaces, k);
	}

	public static Set<String> getShingles(String string) {
		return getShingles(string, DEFAULT_K);
	}

	public static Set<String> getShingles(String string, int k) {
		if (k <= 0)
			throw new IllegalArgumentException(String.format("k should be positive (provided value: %d)", k));
		if (string == null || string.isEmpty())
			return Collections.emptySet();
		Set<String> shingles = SPACE_PATTERN.splitAsStream(string).flatMap(word -> {
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

	public static void main(String[] args) {
		System.out.println(getShingles("Bonjour   monsieur, comment allez-vous aujourd'hui ?", 3));
		System.out.println(getShinglesSpacesRemoved("Bonjour   monsieur, comment allez-vous aujourd'hui ?", 3));
	}

}
