package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.cv.utils.OCRPlasty.RANSAC;

public class OCRPlastyScorer {

	private static Random rand = new Random(System.currentTimeMillis());

	public static void main(String[] args) {
		List<String> strings = getReferenceStrings();
		int idx = rand.nextInt(strings.size());
		String original = strings.get(idx);
		List<String> results = getMutatedStrings(original, 10);
		results.forEach(System.out::println);

		String corrected = getCorrectedString(results);
		double similarity = getSimilarity(original, corrected);
		System.out.println(String.format("Original = %s\nCorrected = %s\nSimilarity = %.3f", original, corrected, similarity));
	}

	private static List<String> getReferenceStrings() {
		List<String> strings = new ArrayList<>();
		strings.add("NOM :");
		strings.add("The quick, brown fox jumps over a lazy dog");
		strings.add("But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system");
		strings.add("The European languages are members of the same family.");
		strings.add("Their separate existence is a myth");
		strings.add("For science, music, sport, etc, Europe uses the same vocabulary");
		strings.add("The languages only differ in their grammar, their pronunciation and their most common words.");
		strings.add("One morning, when Gregor Samsa woke from troubled dreams, he found himself transformed in his bed into a horrible vermin.");
		strings.add("He lay on his armour-like back, and if he lifted his head a little he could see his brown belly, slightly domed and divided by arches into stiff sections.");
		strings.add("A wonderful serenity has taken possession of my entire soul, like these sweet mornings of spring which I enjoy with my whole heart.");
		strings.add("I am alone, and feel the charm of existence in this spot, which was created for the bliss of souls like mine");
		strings.add("Brick quiz whangs jumpy veldt fox. ");
		strings.add("Waltz, bad nymph, for quick jigs vex! Fox nymphs grab quick-jived waltz.");
		strings.add("Far far away, behind the word mountains, far from the countries Vokalia and Consonantia, there live the blind texts");
		strings.add("Separated they live in Bookmarksgrove right at the coast of the Semantics, a large language ocean.");
		strings.add(" A small river named Duden flows by their place and supplies it with the necessary regelialia");
		return strings.stream().map(s -> s.trim()).collect(Collectors.toList());
	}

	private static double getSimilarity(String string1, String string2) {
		return OCRPlasty.similarity(Arrays.asList(string1, string2));
	}

	private static List<String> getMutatedStrings(String string, int size) {
		List<String> results = new ArrayList<>(size + 1);
		IntStream.rangeClosed(0, size).forEach(i -> {
			int maxMutations = 1 + rand.nextInt((int) (1 + Math.round(string.length() * 0.05)));
			results.add(RandomStringMutator.mutate(string, maxMutations));
		});
		return results;
	}

	private static String getCorrectedString(List<String> labels) {
		try {
			return OCRPlasty.correctStrings(labels, RANSAC.LCS);
		} catch (Exception e) {
			System.err.println("Unable to get a RANSAC model");
			return OCRPlasty.correctStrings(labels, RANSAC.NONE);
		}
	}

}
