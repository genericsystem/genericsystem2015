package org.genericsystem.cv.utils;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.testng.Assert.assertEquals;

import java.util.Optional;

import org.opencv.core.Rect;
import org.testng.annotations.Test;

public class RectangleToolsTest {

	private Rect rect1 = new Rect(0, 0, 10, 10);
	private Rect rect2 = new Rect(1, 1, 20, 10);
	private Rect rect3 = new Rect(20, 10, 10, 10);
	private Rect rect4 = new Rect(1, 1, 1, 1);

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
}
