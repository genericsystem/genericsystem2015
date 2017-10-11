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
			if (random < 0.3) {
				// do mutation
				sb.replace(randIdx, randIdx, getRandomString(mutationLength));
			} else if (random < 0.6) {
				// do insertion
				sb.insert(randIdx, getRandomString(mutationLength));
			} else if (random < 0.9) {
				// do deletion
				int max = randIdx + mutationLength >= sb.length() ? randIdx : randIdx + mutationLength;
				sb.delete(randIdx, max);
			} else {
				// do a harsh modification
				int max = randIdx + sb.length() / 2 >= sb.length() ? sb.length() : randIdx + sb.length() / 2;
				sb.replace(randIdx, max, getRandomString(mutationLength));
				sb.append(getRandomString(mutationLength * 2));
				int tmp = rand.nextInt(sb.length());
				max = tmp + sb.length() / 2 > sb.length() ? sb.length() : tmp + sb.length() / 2;
				sb.delete(tmp, max);
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