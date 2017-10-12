package org.genericsystem.cv.utils;

import static org.testng.Assert.assertEquals;

import org.testng.annotations.Test;

public class LetterPairSimilarityTest {

	@Test
	public void compareStrings() {
		String ref = "healed";
		assertEquals(LetterPairSimilarity.compareStrings("", ""), 1d, 0.0);
		assertEquals(LetterPairSimilarity.compareStrings("kitten", ""), 0, 0.0);
		assertEquals(LetterPairSimilarity.compareStrings("", "kitten"), 0, 0.0);
		assertEquals(LetterPairSimilarity.compareStrings(ref, ref), 1d, 0.0);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "sealed"), 0.800, 0.001);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "healthy"), 0.545, 0.001);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "heard"), 0.444, 0.001);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "herded"), 0.400, 0.001);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "help"), 0.250, 0.001);
		assertEquals(LetterPairSimilarity.compareStrings(ref, "sold"), 0, 0.001);
	}

	@Test(expectedExceptions = IllegalArgumentException.class)
	public void compareStringsNullString() {
		LetterPairSimilarity.compareStrings("kitten", null);
	}

	@Test(expectedExceptions = IllegalArgumentException.class)
	public void compareStringsNullString2() {
		LetterPairSimilarity.compareStrings(null, "kitten");
	}
}
