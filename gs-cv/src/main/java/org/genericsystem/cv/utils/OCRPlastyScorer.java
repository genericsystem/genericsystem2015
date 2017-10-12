package org.genericsystem.cv.utils;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.DoubleStream;
import java.util.stream.IntStream;

import org.genericsystem.cv.utils.OCRPlasty.RANSAC;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * OCRPlastyScorer can be used to compute the efficiency of OCRPlasty, depending on the RANSAC method applied to the list of strings given as argument. <br>
 * It uses a list of strings (see {@link #getReferenceStrings()}) that are randomly modified to generate a list of potential candidates (similar to what one get after OCR of the same field across several images). These lists are then submitted to
 * {@link OCRPlasty} to get a corrected string, which is compared to the one used to generate the random list. The results store the similarity between the two strings, and the duration of the computation.
 * 
 * @author Pierrik Lassalas
 */
public class OCRPlastyScorer {

	private static final double MUTATION_PERCENTAGE = 0.05;
	private static Random rand = new Random(System.currentTimeMillis());
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] args) {
		// List<String> strings = getReferenceStrings();
		// int idx = rand.nextInt(strings.size());
		// String original = strings.get(idx);
		// computeScore(original).forEach(res -> res.printResults());
		computeAll();
	}

	/**
	 * Run all the computations: each string will be randomly modified, then subjected to each algorithm defined in {@link OCRPlasty.RANSAC} for correction.
	 */
	private static void computeAll() {
		List<Results> total = new ArrayList<>();
		for (String s : getReferenceStrings()) {
			List<Results> results = computeScore(s);
			total.addAll(results);
		}

		for (RANSAC option : RANSAC.values()) {
			List<Double> scores = total.stream().filter(res -> option.name().equals(res.getRansacMethod())).map(Results::getSimilarity).collect(Collectors.toList());
			Statistics scoreStat = new Statistics(scores);
			logger.info("SCORE for {} {}", option.name(), scoreStat.format());

			List<Double> durations = total.stream().filter(res -> option.name().equals(res.getRansacMethod())).map(res -> Long.valueOf(res.getDuration()).doubleValue() / res.getOriginal().length()).map(x -> x / 1_000_000).collect(Collectors.toList());
			Statistics durationStat = new Statistics(durations);
			logger.info("DURATION (PER CHAR) for {} (ms) {}", option.name(), durationStat.format());
		}
	}

	/**
	 * Perform the computation for a given String, and store the results as a {@link Results} object.
	 * 
	 * @param testString - the original string
	 * @return a List of {@link Results} containing the results of each correction
	 */
	private static List<Results> computeScore(String testString) {
		List<Results> results = new ArrayList<>();

		String corrected;
		List<String> mutateds = getMutatedStrings(testString, 10);

		mutateds.forEach(System.err::println);

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

	/**
	 * Get the list of reference strings used for computation.
	 * 
	 * @return the lsit of reference strings
	 */
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

	/**
	 * Get the similarity between two strings, as defined in {@link OCRPlasty}.
	 * 
	 * @param string1 - the first string
	 * @param string2 - the second string
	 * @return the similarity between the strings, between 0.0 and 1.0
	 */
	private static double getSimilarity(String string1, String string2) {
		return OCRPlasty.similarity(Arrays.asList(string1, string2));
	}

	/**
	 * Apply random modifications on a given string using {@link RandomStringMutator}.
	 * 
	 * @param string - the original string
	 * @param size - the size of the {@link List} that will be generated
	 * @return - a list of randomly modified strings
	 */
	private static List<String> getMutatedStrings(String string, int size) {
		List<String> results = new ArrayList<>(size + 1);
		results.add(".");
		IntStream.rangeClosed(0, size).forEach(i -> {
			int maxMutations = 1 + rand.nextInt((int) (1 + Math.round(string.length() * MUTATION_PERCENTAGE)));
			results.add(RandomStringMutator.mutate(string, maxMutations));
		});
		return results;
	}

	/**
	 * Get the corrected version of a string from a list of approximate strings.
	 * 
	 * @param labels - the list of strings candidates
	 * @param options - one of {@link RANSAC} values, which represents the algorithm used to compute the error in the RANSAC model
	 * @return the corrected string
	 */
	private static String getCorrectedString(List<String> labels, RANSAC options) {
		try {
			return OCRPlasty.correctStrings(labels, options);
		} catch (Exception e) {
			System.err.println("Unable to get a RANSAC model");
			return OCRPlasty.correctStrings(labels, RANSAC.NONE);
		}
	}

	/**
	 * Utility class used to store the results of the computations.
	 * 
	 * @author Pierrik Lassalas
	 */
	public static class Results {
		private long duration;
		private double similarity;
		private String original;
		private String corrected;
		private String ransacMethod;

		public Results(String ransacMethod, long duration, double similarity, String original, String corrected) {
			this.duration = duration;
			this.similarity = similarity;
			this.original = original;
			this.corrected = corrected;
			this.ransacMethod = ransacMethod;
		}

		public void printResults() {
			StringBuffer sb = new StringBuffer();
			sb.append("\n");
			sb.append("--------------------------------------------------------").append("\n");
			sb.append("Method: ").append(ransacMethod).append("\n");
			sb.append("\n");
			sb.append("-> similarity = ").append(similarity).append("\n");
			sb.append("-> duration = ").append(String.format("%,d ms", duration / 1_000_000)).append("\n");
			sb.append("\n");
			sb.append("Original:").append("\n").append(original).append("\n");
			sb.append("Corrected:").append("\n").append(corrected).append("\n");
			sb.append("--------------------------------------------------------").append("\n");
			logger.info(sb.toString());
		}

		public long getDuration() {
			return duration;
		}

		public double getSimilarity() {
			return similarity;
		}

		public String getOriginal() {
			return original;
		}

		public String getCorrected() {
			return corrected;
		}

		public String getRansacMethod() {
			return ransacMethod;
		}
	}

	/**
	 * Utility class used to compute some basic statistics of a {@link List<Double>}
	 * 
	 * @author Pierrik Lassalas
	 */
	public static class Statistics {
		private List<Double> values;
		private boolean computed;
		private double min;
		private double max;
		private double average;
		private double sd;
		private double median;

		public Statistics(List<Double> values) {
			this.values = values;
			compute();
			this.computed = true;
		}

		private void compute() {
			Collections.sort(values, (d1, d2) -> Double.compare(d1, d2));
			this.min = values.get(0);
			this.max = values.get(values.size() - 1);
			this.average = values.stream().mapToDouble(x -> x).average().getAsDouble();
			this.sd = Math.sqrt(values.stream().mapToDouble(x -> Math.pow(x - this.average, 2)).average().getAsDouble());
			int middle = values.size() / 2;
			this.median = middle % 2 == 1 ? values.get(middle) : DoubleStream.of(values.get(middle), values.get(middle - 1)).average().getAsDouble();
		}

		public String format() {
			if (!computed)
				compute();
			StringBuffer sb = new StringBuffer();
			sb.append("\n");
			sb.append(String.format("-> %10s = %.3f", "average", average)).append("\n");
			sb.append(String.format("-> %10s = %.3f", "median", median)).append("\n");
			sb.append(String.format("-> %10s = %.3f", "min", min)).append("\n");
			sb.append(String.format("-> %10s = %.3f", "max", max)).append("\n");
			sb.append(String.format("-> %10s = %.3f", "sd", sd)).append("\n");
			return sb.toString();
		}

		public double getMin() {
			if (!computed)
				compute();
			return min;
		}

		public double getMax() {
			if (!computed)
				compute();
			return max;
		}

		public double getAverage() {
			if (!computed)
				compute();
			return average;
		}

		public double getSd() {
			if (!computed)
				compute();
			return sd;
		}

		public double getMedian() {
			if (!computed)
				compute();
			return median;
		}

	}

}
