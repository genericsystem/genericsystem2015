package org.genericsystem.cv.utils;

public class Levenshtein {

	/**
	 * Computes the Levenshtein distance between two strings.
	 * 
	 * @param a - the first string
	 * @param b - the second string
	 * @return an {@code int} representing the cost
	 */
	public static int distance(String a, String b) {
		if (null == a || null == b)
			throw new IllegalArgumentException("Levenshtein distance requires two not null strings");
		if (a.equals(b))
			return 0;
		if (a.isEmpty())
			return b.length();
		if (b.isEmpty())
			return a.length();

		a = a.toLowerCase();
		b = b.toLowerCase();
		// i == 0
		int[] costs = new int[b.length() + 1];
		for (int i = 0; i < costs.length; i++)
			costs[i] = i;

		for (int i = 1; i <= a.length(); i++) {
			// j == 0; nw = lev(i - 1, j)
			costs[0] = i;
			int nw = i - 1;
			for (int j = 1; j <= b.length(); j++) {
				int cj = Math.min(1 + Math.min(costs[j], costs[j - 1]), a.charAt(i - 1) == b.charAt(j - 1) ? nw : nw + 1);
				nw = costs[j];
				costs[j] = cj;
			}
		}
		return costs[b.length()];
	}
}
