package org.genericsystem.cv.utils;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import org.genericsystem.cv.utils.Ransac.Model;
import org.genericsystem.reinforcer.tools.LetterPairSimilarity;
import org.genericsystem.reinforcer.tools.Levenshtein;
import org.genericsystem.reinforcer.tools.StringCompare;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * This class is used to compute the best possible string from a given list of closely-related strings. <br>
 * Its main use is to get a consensus of the correct string from a given list of OCR text. <br>
 * The list will be evaluated to look for the LCS (longest linear subsequence), and the characters between the characters of the LCS will be estimated from all the strings.
 */
public class OCRPlasty {

	/**
	 * This enum contains all the methods available to compute an error with the RANSAC model.
	 */
	public static enum RANSAC {
		NONE,
		LCS,
		DIVERSITY,
		LEVENSHTEIN,
		NORM_LEVENSHTEIN
	}

	/**
	 * Utility class used to return two results : an {@link Optional} string, representing the corrected string, a {@link Set} strings, representing the outliers eliminated by the RANSAC, and the confidence (percentage of similarity between inlier
	 * strings).
	 */
	public static class Tuple {
		private final String string;
		private final Set<String> outliers;
		private final double confidence;

		public Tuple(String string, Set<String> outliers, double confidence) {
			this.string = string;
			this.outliers = outliers;
			this.confidence = confidence;
		}

		public String getString() {
			return string;
		}

		public Set<String> getOutliers() {
			return outliers;
		}

		public double getConfidence() {
			return confidence;
		}
	}

	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());

	public static void main(String[] args) {
		List<String> labels = new ArrayList<>();
		labels.add("had I expressed the agony I frequentl felt he would have been taught to long for its alleviati");
		labels.add("gad I sed the agony I fefjuently felt he would have been to long for its alleviafcion");
		labels.add("had I expressed tbe agony I frejuently felt he would have been taught to long for its alleviationq");
		labels.add("had I expresset th agny I frequently feltu he wouald have ben taufht to lng fr its alevation");
		labels.add("had I # tly feltu he wouald have ben taufht to lng fr iets alevation");
		labels.add("fger gezrgze ertg");
		labels.add("");
		labels.add(".");

		for (RANSAC option : RANSAC.values()) {
			System.out.println(option.name());
			System.out.println(correctStrings(new ArrayList<>(labels), option));
			// System.out.println(correctStringsAndGetOutliers(new ArrayList<>(labels), option));
			for (StringCompare.SIMILARITY method : StringCompare.SIMILARITY.values()) {
				System.out.println(String.format("Similarity (%s): %.3f", method.name(), StringCompare.similarity(labels, method)));
			}

		}
	}

	public static String correctStrings(List<String> labels, OCRPlasty.RANSAC options) {
		return correctStrings(labels, options, StringCompare.SIMILARITY.LEVENSHTEIN);
	}

	/**
	 * Get a corrected String from a given list of strings.
	 * 
	 * @param labels - the list of string that will be 'averaged'
	 * @param options - one of the value of {@link RANSAC} enum, which represents the algorithm used to compute the error in the RANSAC model
	 * @param method - the method to be used to compute string similarity
	 * @return an {@link Optional} containing the string that reached the best consensus, otherwise an empty {@link Optional}
	 */
	public static String correctStrings(List<String> labels, OCRPlasty.RANSAC options, StringCompare.SIMILARITY method) {
		Tuple res = doStringCorrection(labels, options, method, false);
		return res.getString();
	}

	public static Tuple correctStringsAndGetOutliers(List<String> labels, OCRPlasty.RANSAC options) {
		return doStringCorrection(labels, options, StringCompare.SIMILARITY.LEVENSHTEIN, true);
	}

	/**
	 * Get a corrected String from a given list of strings, along with a {@link List} of outliers if a RANSAC method was used.
	 * 
	 * @param labels - the list of string that will be 'averaged'
	 * @param options - one of the value of {@link RANSAC} enum, which represents the algorithm used to compute the error in the RANSAC model
	 * @param method - the method to be used to compute string similarity
	 * @return a {@link Tuple} object with the results
	 */
	//	public static Tuple correctStringsAndGetOutliers(List<String> labels, OCRPlasty.RANSAC options, StringCompare.SIMILARITY method) {
	//		return doStringCorrection(labels, options, method, true);
	//	}

	public static Ransac<String> getLabelRansac(List<String> labels, double error){
		//		List<String> trimmed = labels.stream().map(s -> s.trim()).filter(s -> s.length() > 0).collect(Collectors.toList());
		int minSize = 1 + labels.size() / 2;
		if (minSize < 2)
			return null;
		Ransac<String> ransac = null;		
		for (int i = 1;  i <= 10; ++i) {
			try {
				ransac = new Ransac<>(labels, getModelProviderNormLevenshtein(), 2, 10 * i, error, minSize);
			} catch (Exception e) {
				error *= 1.5;
				logger.trace("Can't get a good model. Increase the error margin to {}", error);
			}
		}
		return ransac;
	}

	/**
	 * Performs the string correction.
	 * 
	 * @param labels - the list of string that will be 'averaged'
	 * @param options - one of the value of {@link RANSAC} enum, which represents the algorithm used to compute the error in the RANSAC model
	 * @param method - the method to be used to compute string similarity
	 * @param needOutliers - true if the list of outliers eliminated by the RANSAC is needed, false otherwise
	 * @return a {@link Tuple} object with the results
	 */
	private static Tuple doStringCorrection(List<String> labels, OCRPlasty.RANSAC options, StringCompare.SIMILARITY method, boolean needOutliers) {
		// Trim all the elements of the list
		List<String> trimmed = labels.stream().map(s -> s.trim()).filter(s -> s.length() > 0).collect(Collectors.toList());
		// Initialize the parameters
		Function<Collection<String>, Model<String>> modelProvider = null;
		Set<String> outliers = Collections.emptySet();
		String result = "";
		double confidence = 0;
		double error = 1;

		switch (options) {
		default:
		case NONE:
			result = ocrPlasty(trimmed);
			confidence = StringCompare.similarity(trimmed, method);
		case LCS:
			modelProvider = getModelProviderMaxLcs();
			error = 1d;
			break;
		case DIVERSITY:
			modelProvider = getModelProviderDiversity();
			error = 0.1;
			break;
		case LEVENSHTEIN:
			modelProvider = getModelProviderLevenshtein();
			error = 1d;
			break;
		case NORM_LEVENSHTEIN:
			modelProvider = getModelProviderNormLevenshtein();
			error = 0.1;
			break;
		}

		if (modelProvider != null) { // One of the RANSAC methods has been called
			List<String> inliers = getRansacInliers(trimmed, modelProvider, error);
			Set<String> inliersSet = new HashSet<>(inliers); // Save a Set copy to be able to get the outliers if needed
			confidence = StringCompare.similarity(inliers, method);
			// Compute the string alignment
			result = inliers.isEmpty() ? ocrPlasty(trimmed) : ocrPlasty(inliers);
			// If no inliers were found or if we don't need the outliers, return an empty list
			if (inliers.isEmpty() || !needOutliers) {
				outliers = Collections.emptySet();
			} else { // Otherwise get the set difference (elements present in trimmed but not in inliersSet)
				Map<Boolean, Set<String>> partitionnedMap = trimmed.stream().collect(Collectors.partitioningBy(s -> inliersSet.contains(s), Collectors.toSet()));
				outliers = partitionnedMap.get(false);
			}
		}

		return new Tuple(result, outliers, confidence);
	}

	/**
	 * Attempt to provide a corrected string from a list of string candidates.
	 * 
	 * @param labels - the list of strings
	 * @return an {@link Optional} containing the string if it was found, otherwise an empty {@link Optional}
	 */
	public static String ocrPlasty(List<String> labels) {
		if (labels == null)
			throw new IllegalArgumentException("The list cannot be null");
		if (labels.isEmpty())
			return null;
		String common = longestCommonSubsequence(labels);
		String consensus = "";
		for (int i = 0; i < common.length() + 1; i++) {
			List<String> candidates = new ArrayList<>();
			for (int label = 0; label < labels.size(); label++) {
				List<String> is = (i < common.length()) ? interString(labels.get(label), common.charAt(i)) : endString(labels.get(label));
				labels.set(label, is.get(0));
				candidates.add(is.get(1));
			}
			consensus += selectBest(candidates);
			if (i < common.length() - 1)
				consensus += common.charAt(i);
		}
		return consensus.isEmpty() ? null : consensus;
	}

	/**
	 * Compute a limited set of strings from a given list, eliminating the outliers using a RANSAC algorithm.
	 * 
	 * @param labels - the list of strings
	 * @param modelProvider - the model provider (see {@link Ransac})
	 * @param error - the error margin used in the RANSAC to determine whether a value is considered in the model
	 * @return a new list of strings consisting without the outliers
	 */
	private static List<String> getRansacInliers(List<String> labels, Function<Collection<String>, Model<String>> modelProvider, double error) {
		if (labels.isEmpty())
			return Collections.emptyList();

		int minSize = 1 + labels.size() / 2;
		if (minSize < 2)
			return Collections.emptyList();

		Map<Integer, String> bestFit = new HashMap<>();
		for (int i = 1, maxAttempts = 10; bestFit.size() <= 3 && i <= maxAttempts; ++i) {
			try {
				Ransac<String> ransac = new Ransac<>(labels, modelProvider, 2, 10 * i, error, minSize);
				bestFit = ransac.getBestDataSet();
				// bestFit.entrySet().forEach(entry -> logger.debug("key: {} | | value: {}", entry.getKey(), entry.getValue()));
			} catch (Exception e) {
				error *= 1.5;
				logger.trace("Can't get a good model. Increase the error margin to {}", error);
			}
		}
		return bestFit.values().stream().collect(Collectors.toList());
	}

	/**
	 * Get a model based on the maximization of the LCS length.
	 * 
	 * @return the model
	 */
	private static Function<Collection<String>, Model<String>> getModelProviderMaxLcs() {
		return datas -> {
			Iterator<String> it = datas.iterator();
			String subsequence = null;
			if (it.hasNext())
				subsequence = it.next();
			while (it.hasNext()) {
				String label = it.next();
				if (!(subsequence.isEmpty() || label.isEmpty()))
					subsequence = lcs(subsequence, label);
			}
			String common = subsequence;

			return new OcrModel() {
				@Override
				public double computeError(String data) {
					error = Levenshtein.distance(data, common);
					return error;
				}
			};
		};
	}

	/**
	 * Get a model based on the maximization of the similarity (decreasing the diversity).
	 * 
	 * @return the model
	 */
	private static Function<Collection<String>, Model<String>> getModelProviderDiversity() {
		return datas -> {
			return new OcrModel() {
				@Override
				public double computeError(String data) {
					error = 0d;
					for (String s : datas) {
						double sim = LetterPairSimilarity.compareStrings(data, s);
						error += 1 - sim; // return the 'diversity' instead of the similarity
					}
					return error / datas.size();
				}
			};
		};
	}

	/**
	 * Get a model based on the minimization of the Levenshtein distance.
	 * 
	 * @return the model
	 */
	private static Function<Collection<String>, Model<String>> getModelProviderLevenshtein() {
		return datas -> {
			return new OcrModel() {
				@Override
				public double computeError(String data) {
					error = 0d;
					for (String s : datas) {
						error += Levenshtein.distance(data, s);
					}
					return error / datas.size();
				}
			};
		};
	}

	/**
	 * Get a model based on the minimization of the normalized Levenshtein distance.
	 * 
	 * @return the model
	 */
	private static Function<Collection<String>, Model<String>> getModelProviderNormLevenshtein() {
		return datas -> {
			return new OcrModel() {
				@Override
				public double computeError(String data) {
					error = 0d;
					for (String s : datas) {
						error += Levenshtein.normedDistance(data, s);
					}
					return error / datas.size();
				}
			};
		};
	}

	/**
	 * Custom {@link Model} used in the {@link OCRPlasty} class. <br>
	 */
	public static abstract class OcrModel implements Model<String> {
		/**
		 * Computed local error.
		 */
		protected double error = 0d;

		/**
		 * Compute the global error (sum of the square of each individual error)
		 */
		@Override
		public double computeGlobalError(List<String> datas, Collection<String> consensusDatas) {
			double globalError = 0d;
			for (String s : consensusDatas) {
				globalError += Math.pow(computeError(s), 2d);
			}
			return Math.sqrt(globalError) / consensusDatas.size();
		}

		/**
		 * Best error
		 */
		@Override
		public Object[] getParams() {
			return new Object[] { error };
		}
	}

	/**
	 * Select the best string candidate from a list.
	 * 
	 * @param candidates - the list of strings
	 * @return the best candidate
	 */
	private static String selectBest(List<String> candidates) {
		Map<String, Long> occurrences = candidates.stream().collect(Collectors.groupingBy(s -> s, Collectors.counting()));
		long maxOcc = Collections.max(occurrences.values());
		if (maxOcc > 1)
			return occurrences.entrySet().stream().filter(entry -> entry.getValue().equals(maxOcc)).findFirst().map(e -> e.getKey()).orElse(leastDifferent(candidates));
		else
			return leastDifferent(candidates);
	}

	/**
	 * String between two consecutive elements of the LCS
	 * 
	 * @param string - the string in which the search is performed
	 * @param c - the character in the LCS
	 * @return a list with the cropped string as the first element, and the 'interstring' as the second element
	 */
	private static List<String> interString(String string, char c) {
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

	/**
	 * String following the last element of the LCS
	 * 
	 * @param string - the string in which the search is performed
	 * @return a list with an empty string as the first element, and the string as the second element
	 */
	private static List<String> endString(String string) { // string following the last element of the lcs
		List<String> is = new ArrayList<>();
		is.add("");
		is.add(string);
		return is;
	}

	private static String leastDifferent(List<String> strings) { // "least different" string from the others (smallest sum of Levenshtein distance with the others)
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

	/**
	 * Compute the LCS distance between each element of a list
	 * 
	 * @param labels - a list of strings
	 * @return the LCS
	 */
	private static String longestCommonSubsequence(List<String> labels) {
		String subsequence = labels.get(0).trim();
		for (int i = 1; i < labels.size(); i++) {
			if (!(subsequence.isEmpty() || labels.get(i).trim().isEmpty()))
				subsequence = lcs(subsequence, labels.get(i).trim());
		}
		return subsequence;
	}

	/**
	 * Compute the LCS between two strings
	 * 
	 * @param stringX - the first string
	 * @param stringY - the second string
	 * @return the LCS
	 */
	private static String lcs(String stringX, String stringY) {
		int m = stringX.length();
		int n = stringY.length();
		int[][] mat = new int[m + 1][n + 1];
		// Following steps build mat[m+1][n+1] in bottom up fashion. Note that mat[i][j] contains length of LCS of X[0..i-1] and Y[0..j-1]
		for (int i = 0; i <= m; i++) {
			for (int j = 0; j <= n; j++) {
				if (i == 0 || j == 0)
					mat[i][j] = 0;
				else if (stringX.charAt(i - 1) == stringY.charAt(j - 1))
					mat[i][j] = mat[i - 1][j - 1] + 1;
				else
					mat[i][j] = Math.max(mat[i - 1][j], mat[i][j - 1]);
			}
		}
		// Following code is used to print LCS
		int index = mat[m][n];
		int temp = index;
		// Create a character array to store the lcs string
		char[] lcs = new char[index + 1];
		lcs[index] = '\0'; // Set the terminating character
		// Start from the right-most-bottom-most corner and one by one store characters in lcs[]
		int i = m, j = n;
		while (i > 0 && j > 0) {
			// If current character in X[] and Y are same, then current character is part of LCS
			if (stringX.charAt(i - 1) == stringY.charAt(j - 1)) {
				// Put current character in result
				lcs[index - 1] = stringX.charAt(i - 1);
				// reduce values of i, j and index
				i--;
				j--;
				index--;
			}
			// If not same, then find the larger of two and go in the direction of larger value
			else if (mat[i - 1][j] > mat[i][j - 1])
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
