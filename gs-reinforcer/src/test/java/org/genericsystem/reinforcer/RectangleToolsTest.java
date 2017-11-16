package org.genericsystem.reinforcer;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.assertNotNull;
import static org.testng.Assert.assertNull;
import static org.testng.Assert.assertTrue;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.genericsystem.reinforcer.tools.GSPoint;
import org.genericsystem.reinforcer.tools.GSRect;
import org.genericsystem.reinforcer.tools.RectangleTools;
import org.testng.annotations.Test;

public class RectangleToolsTest {

	private GSRect rect1 = new GSRect(0, 0, 10, 10);
	private GSRect rect2 = new GSRect(1, 1, 20, 10);
	private GSRect rect3 = new GSRect(20, 10, 10, 10);
	private GSRect rect4 = new GSRect(1, 1, 1, 1);

	@Test
	public void nonMaximumSuppression() {
		// Test with empty / null
		List<GSRect> list = RectangleTools.nonMaximumSuppression(Collections.emptyList(), 0.3);
		assertTrue(list.isEmpty());
		list = RectangleTools.nonMaximumSuppression(null, 0.3);
		assertTrue(list.isEmpty());

		// Test case 1: should return 1 rect
		GSRect r1 = new GSRect(new GSPoint(12, 84), new GSPoint(140, 212));
		GSRect r2 = new GSRect(new GSPoint(24, 84), new GSPoint(152, 212));
		GSRect r3 = new GSRect(new GSPoint(36, 84), new GSPoint(164, 212));
		GSRect r4 = new GSRect(new GSPoint(12, 96), new GSPoint(140, 224));
		GSRect r5 = new GSRect(new GSPoint(24, 96), new GSPoint(152, 224));
		GSRect r6 = new GSRect(new GSPoint(24, 108), new GSPoint(152, 236));

		GSRect simplified1 = new GSRect(24, 108, 128, 128);
		list = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3, r4, r5, r6), 0.3);
		assertTrue(!list.isEmpty());
		assertEquals(list, Arrays.asList(simplified1));

		// Test 2: should return 1 rect
		r1 = new GSRect(new GSPoint(114, 60), new GSPoint(178, 124));
		r2 = new GSRect(new GSPoint(120, 60), new GSPoint(184, 124));
		r3 = new GSRect(new GSPoint(114, 66), new GSPoint(178, 130));

		GSRect simplified2 = new GSRect(114, 66, 64, 64);
		list = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3), 0.3);
		assertTrue(!list.isEmpty());
		assertEquals(list, Arrays.asList(simplified2));

		// Test case 3: should return 2 rects
		r1 = new GSRect(new GSPoint(12, 30), new GSPoint(76, 94));
		r2 = new GSRect(new GSPoint(12, 36), new GSPoint(76, 100));
		r3 = new GSRect(new GSPoint(72, 36), new GSPoint(200, 164));
		r4 = new GSRect(new GSPoint(84, 48), new GSPoint(212, 176));
		GSRect simplified31 = new GSRect(12, 36, 64, 64);
		GSRect simplified32 = new GSRect(84, 48, 128, 128);

		list = RectangleTools.nonMaximumSuppression(Arrays.asList(r1, r2, r3, r4), 0.3);
		assertTrue(!list.isEmpty());
		assertEquals(list, Arrays.asList(simplified31, simplified32));
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
	public void inclusiveArea() {
		GSRect r1 = new GSRect(0, 0, 100, 100);
		GSRect r2 = new GSRect(49, 0, 50, 100);
		GSRect r3 = new GSRect(100, 0, 100, 100);

		assertEquals(r1.inclusiveArea(r1), 1, 0.001);
		assertEquals(r1.inclusiveArea(r2), 0.5, 0.001);
		assertEquals(r1.inclusiveArea(r3), 0, 0.001);
	}

	@Test
	public void getIntersection() {
		GSRect inter12 = new GSRect(1, 1, 9, 9);

		GSRect inter = rect1.getIntersection(rect1);
		assertNotNull(inter);
		assertEquals(inter, rect1);

		inter = rect1.getIntersection(rect2);
		assertNotNull(inter);
		assertEquals(inter, inter12);

		inter = rect1.getIntersection(rect3);
		assertNull(inter);

		inter = rect1.getIntersection(rect4);
		assertNotNull(inter);
		assertEquals(inter, rect4);
	}

	@Test
	public void getUnion() {
		GSRect union12 = new GSRect(0, 0, 21, 11);
		GSRect union13 = new GSRect(0, 0, 30, 20);

		GSRect union = rect1.getUnion(rect1);
		assertEquals(union, rect1);

		union = rect1.getUnion(rect2);
		assertEquals(union, union12);

		union = rect1.getUnion(rect3);
		assertEquals(union, union13);

		union = rect1.getUnion(rect4);
		assertEquals(union, rect1);
	}

	@Test
	public void isOverlapping() {
		assertTrue(rect1.isOverlapping(rect1));
		assertTrue(rect1.isOverlapping(rect2));
		assertFalse(rect1.isOverlapping(rect3));
		assertTrue(rect1.isOverlapping(rect4));
	}

	@Test(expectedExceptions = { IllegalArgumentException.class })
	public void isOverlappingNull() {
		rect1.isOverlapping(null);
	}
}
