package org.genericsystem.cv.utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.cv.utils.OCRPlasty.RANSAC;

public class OCRPlastyScorer {

	private static final double MUTATION_PERCENTAGE = 0.08;
	private static Random rand = new Random(System.currentTimeMillis());

	public static void main(String[] args) {
		List<String> strings = getReferenceStrings();
		int idx = rand.nextInt(strings.size());
		String original = strings.get(idx);
		computeScore(original).forEach(res -> res.printResults());
	}

	private static List<Results> computeScore(String testString) {
		List<Results> results = new ArrayList<>();

		String corrected;
		List<String> mutateds = getMutatedStrings(testString, 10);

		for (RANSAC option : RANSAC.values()) {
			long start = System.nanoTime();
			corrected = getCorrectedString(mutateds, option);
			long stop = System.nanoTime();

			double similarity = getSimilarity(testString, corrected);
			long duration = stop - start;
			results.add(new Results(option.name(), duration, similarity, testString, corrected));
		}

		return results;
	}

	private static List<String> getReferenceStrings() {
		List<String> strings = new ArrayList<>();
		strings.add("Nom :");
		strings.add("Prénom :");
		strings.add("IDFRABABOUN<<<<<<<<<<<<<<<<<<<772354");
		strings.add("Né(e) le :");
		strings.add("24.07.1976");
		strings.add("PRÉFECTURE DE SEINE-MARITIME (76)");
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
			int maxMutations = 1 + rand.nextInt((int) (1 + Math.round(string.length() * MUTATION_PERCENTAGE)));
			results.add(RandomStringMutator.mutate(string, maxMutations));
		});
		return results;
	}

	private static String getCorrectedString(List<String> labels, RANSAC options) {
		try {
			return OCRPlasty.correctStrings(labels, options);
		} catch (Exception e) {
			System.err.println("Unable to get a RANSAC model");
			return OCRPlasty.correctStrings(labels, RANSAC.NONE);
		}
	}

	public static class Results {
		private long duration;
		private double score;
		private String original;
		private String corrected;
		private String ransacMethod;

		public Results(String ransacMethod, long duration, double score, String original, String corrected) {
			this.duration = duration;
			this.score = score;
			this.original = original;
			this.corrected = corrected;
			this.ransacMethod = ransacMethod;
		}

		public void printResults() {
			System.out.println("--------------------------------------------------------");
			System.out.println("Method: " + ransacMethod);
			System.out.println();
			System.out.println("-> similarity = " + score);
			System.out.println(String.format("-> duration = %,d ms", duration / 1_000_000));
			System.out.println();
			System.out.println("Original:");
			System.out.println(original);
			System.out.println("Corrected:");
			System.out.println(corrected);
			System.out.println("--------------------------------------------------------");
		}
	}

}
