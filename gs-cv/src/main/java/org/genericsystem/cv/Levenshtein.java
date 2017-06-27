package org.genericsystem.cv;

public class Levenshtein {

	/**
	 * Computes the Levenshtein distance between two strings.
	 * 
	 * @param a - the first string
	 * @param b - the second string
	 * @return an {@code int} representing the cost 
	 */
	public static int distance(String a, String b) {
		a = a.toLowerCase();
		b = b.toLowerCase();
		// i == 0
		int[] costs = new int[b.length() + 1];
		for (int j = 0; j < costs.length; j++)
			costs[j] = j;
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
//		System.out.print(">>> Levenshtein a : \"" + a + "\"" + " | b : \"" + b + "\"");
//		System.out.println(" | cost : " + costs[b.length()]);
		return costs[b.length()];
	}

	public static void main(String[] args) {
		String[] data = { "kitten", "sitting", "saturday", "sunday", "rosettacode", "raisethysword" };
		for (int i = 0; i < data.length; i += 2)
			System.out.println("distance(" + data[i] + ", " + data[i + 1] + ") = " + distance(data[i], data[i + 1]));
	}
}
