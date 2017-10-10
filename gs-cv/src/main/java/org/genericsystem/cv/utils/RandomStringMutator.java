package org.genericsystem.cv.utils;

import java.util.Random;
import java.util.stream.IntStream;

public class RandomStringMutator {
	private static Random rand = new Random(System.currentTimeMillis());
	private static final String[] LETTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890éçà$£€µ*ù%!?,.;:/\' ".toLowerCase().split("");

	public static void main(String[] args) {
		System.out.println(mutate("Bonjour, comment ça va ?", 1));
	}

	public static String mutate(String string, int mutationCount) {
		StringBuffer sb = new StringBuffer(string);
		IntStream.range(0, mutationCount).forEach(i -> {
			double random = rand.nextDouble();
			int randIdx = rand.nextInt(sb.length());
			int mutationLength = 1 + rand.nextInt(mutationCount > 2 ? mutationCount / 2 : 1);
			if (random < 0.33) {
				// do mutation
				sb.replace(randIdx, randIdx, getRandomString(mutationLength));
			} else if (random < 0.67) {
				// do insertion
				sb.insert(randIdx, getRandomString(mutationLength));
			} else {
				// do deletion
				int max = randIdx + mutationLength >= sb.length() ? randIdx : randIdx + mutationLength;
				sb.delete(randIdx, max);
			}
		});
		return sb.toString();
	}

	private static String getRandomString(int maxLen) {
		StringBuffer sb = new StringBuffer(maxLen);
		IntStream.range(0, maxLen).forEach(i -> {
			int randIdx = rand.nextInt(LETTERS.length);
			sb.append(LETTERS[randIdx]);
		});
		return sb.toString();
	}
}