package org.genericsystem.cv.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.testng.Assert.assertEquals;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.testng.annotations.Test;

public class RectangleToolsTest {

	private Rect rect1 = new Rect(0, 0, 10, 10);
	private Rect rect2 = new Rect(1, 1, 20, 10);
	private Rect rect3 = new Rect(20, 10, 10, 10);
	private Rect rect4 = new Rect(1, 1, 1, 1);

	@Test
	public void nonMaximumSuppression() {
		// Test with empty / null
		Optional<List<Rect>> optional = RectangleTools.nonMaximumSuppression(Collections.emptyList(), 0.3);
		assertTrue(Optional.empty().equals(optional));
		optional = RectangleTools.nonMaximumSuppression(null, 0.3);
		assertTrue(Optional.empty().equals(optional));

		// Test case 1: should return 1 rect
		Rect r1 = new Rect(new Point(12, 84), new Point(140, 212));
		Rect r2 = new Rect(new Point(24, 84), new Point(152, 212));
		Rect r3 = new Rect(new Point(36, 84), new Point(164, 212));
		Rect r4 = new Rect(new Point(12, 96), new Point(140, 224));
		Rect r5 = new Rect(new Point(24, 96), new Point(152, 224));
		Rect r6 = new Rect(new Point(24, 108), new Point(152, 236));

		Rect simplified1 = new Rect(24, 108, 128, 128);
		optional = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3, r4, r5, r6), 0.3);
		assertTrue(optional.isPresent());
		assertEquals(optional.get(), Arrays.asList(simplified1));

		// Test 2: should return 1 rect
		r1 = new Rect(new Point(114, 60), new Point(178, 124));
		r2 = new Rect(new Point(120, 60), new Point(184, 124));
		r3 = new Rect(new Point(114, 66), new Point(178, 130));

		Rect simplified2 = new Rect(114, 66, 64, 64);
		optional = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3), 0.3);
		assertTrue(optional.isPresent());
		assertEquals(optional.get(), Arrays.asList(simplified2));

		// Test case 3: should return 2 rects
		r1 = new Rect(new Point(12, 30), new Point(76, 94));
		r2 = new Rect(new Point(12, 36), new Point(76, 100));
		r3 = new Rect(new Point(72, 36), new Point(200, 164));
		r4 = new Rect(new Point(84, 48), new Point(212, 176));
		Rect simplified31 = new Rect(12, 36, 64, 64);
		Rect simplified32 = new Rect(84, 48, 128, 128);

		optional = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3, r4), 0.3);
		assertTrue(optional.isPresent());
		assertEquals(optional.get(), Arrays.asList(simplified31, simplified32));
	}

	@Test
	public void commonArea() {
		double[] res = RectangleTools.commonArea(rect1, rect2);
		assertEquals(res[0], 0.81, 0.001);
		assertEquals(res[1], 0.405, 0.001);

		res = RectangleTools.commonArea(rect1, rect1);
		assertEquals(res[0], 1.0, 0.001);
		assertEquals(res[1], 1.0, 0.001);

		res = RectangleTools.commonArea(rect1, rect3);
		assertEquals(res[0], 0, 0.001);
		assertEquals(res[1], 0, 0.001);

		res = RectangleTools.commonArea(rect1, rect4);
		assertEquals(res[0], 0.01, 0.001);
		assertEquals(res[1], 1.0, 0.001);

		res = RectangleTools.commonArea(rect4, rect1);
		assertEquals(res[0], 1.0, 0.001);
		assertEquals(res[1], 0.01, 0.001);
	}

	@Test
	public void getIntersection() {
		Rect inter12 = new Rect(1, 1, 9, 9);

		Optional<Rect> inter = RectangleTools.getIntersection(rect1, rect1);
		assertTrue(inter.isPresent());
		assertEquals(inter.get(), rect1);

		inter = RectangleTools.getIntersection(rect1, rect2);
		assertTrue(inter.isPresent());
		assertEquals(inter.get(), inter12);

		inter = RectangleTools.getIntersection(rect1, rect3);
		assertFalse(inter.isPresent());
		assertTrue(Optional.empty().equals(inter));

		inter = RectangleTools.getIntersection(rect1, rect4);
		assertTrue(inter.isPresent());
		assertEquals(inter.get(), rect4);
	}

	@Test
	public void getUnion() {
		Rect union12 = new Rect(0, 0, 21, 11);
		Rect union13 = new Rect(0, 0, 30, 20);

		Rect union = RectangleTools.getUnion(rect1, rect1);
		assertEquals(union, rect1);

		union = RectangleTools.getUnion(rect1, rect2);
		assertEquals(union, union12);

		union = RectangleTools.getUnion(rect1, rect3);
		assertEquals(union, union13);

		union = RectangleTools.getUnion(rect1, rect4);
		assertEquals(union, rect1);
	}

	@Test
	public void isOverlapping() {
		assertTrue(RectangleTools.isOverlapping(rect1, rect1));
		assertTrue(RectangleTools.isOverlapping(rect1, rect2));
		assertFalse(RectangleTools.isOverlapping(rect1, rect3));
		assertTrue(RectangleTools.isOverlapping(rect1, rect4));
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void isOverlappingNull() {
		RectangleTools.isOverlapping(null, null);
	}
}
