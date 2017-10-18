package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.List;
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
	private static final String SPACE_PATTERN = "\\s+";

	public static List<String> getShinglesSpacesRemoved(String string) {
		return getShinglesSpacesRemoved(string, DEFAULT_K);
	}

	public static List<String> getShinglesSpacesRemoved(String string, int k) {
		String copy = string.replaceAll(SPACE_PATTERN, "");
		return getShingles(copy, k);
	}

	public static List<String> getShingles(String string) {
		return getShingles(string, DEFAULT_K);
	}

	public static List<String> getShingles(String string, int k) {
		Pattern pattern = Pattern.compile(SPACE_PATTERN);
		List<String> shingles = pattern.splitAsStream(string).flatMap(word -> {
			if (word.length() < k)
				return Stream.empty();
			int size = word.length() - k + 1;
			List<String> tmp = new ArrayList<>(size);
			for (int i = 0; i < size; ++i) {
				tmp.add(word.substring(i, i + k));
			}
			return tmp.stream();
		}).collect(Collectors.toList());
		return shingles;
	}

	public static void main(String[] args) {
		System.out.println(getShingles("Bonjour   monsieur, comment allez-vous aujourd'hui ?", 3));
		System.out.println(getShinglesSpacesRemoved("Bonjour   monsieur, comment allez-vous aujourd'hui ?", 3));
	}

}
