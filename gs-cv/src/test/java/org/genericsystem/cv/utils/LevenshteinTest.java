package org.genericsystem.cv.utils;

import static org.testng.Assert.assertEquals;

import org.testng.annotations.Test;

public class LevenshteinTest {

	@Test
	public void distance() {
		assertEquals(Levenshtein.distance("kitten", "sitting"), 3);
		assertEquals(Levenshtein.distance("saturday", "sunday"), 3);
		assertEquals(Levenshtein.distance("rosettacode", "raisethysword"), 8);
		assertEquals(Levenshtein.distance("", ""), 0);
		assertEquals(Levenshtein.distance("kitten", "kitten"), 0);
		assertEquals(Levenshtein.distance("kitten", ""), 6);
		assertEquals(Levenshtein.distance("", "kitten"), 6);
	}

	@Test(expectedExceptions = IllegalArgumentException.class)
	public void testDistanceNullString() {
		Levenshtein.distance("kitten", null);
	}

	@Test(expectedExceptions = IllegalArgumentException.class)
	public void testDistanceNullString2() {
		Levenshtein.distance(null, "kitten");
	}
}
