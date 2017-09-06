package org.genericsystem.layout;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

public class LayoutComparisonTest {
	@Test
	public void testNodeComparison() {
		double x11 = 0.1 + Math.random() * 0.8;
		double x21 = 0.1 + Math.random() * 0.8;
		double y11 = 0.1 + Math.random() * 0.8;
		double y21 = 0.1 + Math.random() * 0.8;

		double x12 = x11 + 0.01 * (x21 - x11);
		double x22 = x21 + 0.01 * (x21 - x11);
		double y12 = y11 + 0.01 * (y21 - y11);
		double y22 = y21 + 0.01 * (y21 - y11);

		Layout l1 = new Layout(null, x11, x21, y11, y21);
		Layout l2 = new Layout(null, x12, x22, y12, y22);

		assert l1.nodeIsEqual(l2, 0.011);
		assert !l1.nodeIsEqual(l2, 0.009);
	}

	@Test
	public void testChildrenCounterparts() {

		// Containing Layout A
		Layout layoutA = new Layout(null, Math.random(), Math.random(), Math.random(), Math.random());

		// 1st child
		double x1A1 = 0.1;
		double x2A1 = 0.6;
		double y1A1 = 0.1;
		double y2A1 = 0.3;
		Layout childA1 = new Layout(layoutA, x1A1, x2A1, y1A1, y2A1);

		// 2nd child
		double x1A2 = 0.7;
		double x2A2 = 0.9;
		double y1A2 = 0.1;
		double y2A2 = 0.3;
		Layout childA2 = new Layout(layoutA, x1A2, x2A2, y1A2, y2A2);

		// 3rd child
		double x1A3 = 0.1;
		double x2A3 = 0.3;
		double y1A3 = 0.4;
		double y2A3 = 0.9;
		Layout childA3 = new Layout(layoutA, x1A3, x2A3, y1A3, y2A3);

		// 4th child
		double x1A4 = 0.4;
		double x2A4 = 0.9;
		double y1A4 = 0.4;
		double y2A4 = 0.9;
		Layout childA4 = new Layout(layoutA, x1A4, x2A4, y1A4, y2A4);

		// 1st child of the 4th child
		double x1A41 = 0.1;
		double x2A41 = 0.5;
		double y1A41 = 0.1;
		double y2A41 = 0.9;
		Layout childA41 = new Layout(childA4, x1A41, x2A41, y1A41, y2A41);

		// 2nd child of the 4th child
		double x1A42 = 0.6;
		double x2A42 = 0.9;
		double y1A42 = 0.1;
		double y2A42 = 0.9;
		Layout childA42 = new Layout(childA4, x1A42, x2A42, y1A42, y2A42);

		// Contained Layout B
		Layout layoutB = new Layout(null, Math.random(), Math.random(), Math.random(), Math.random());

		// 1st child similar (or not) to the 1st child of the containing layout A
		double x1B1 = x1A1 + 0.01 * (x2A1 - x1A1);
		double x2B1 = x2A1 + 0.01 * (x2A1 - x1A1);
		double y1B1 = y1A1 + 0.01 * (y2A1 - y1A1);
		double y2B1 = y2A1 - 0.01 * (y2A1 - y1A1);
		Layout childB1 = new Layout(layoutB, x1B1, x2B1, y1B1, y2B1);

		// 2nd child similar (or not) to the 4th child of the containing layout A
		double x1B2 = x1A4 + 0.01 * (x2A4 - x1A4);
		double x2B2 = x2A4 + 0.01 * (x2A4 - x1A4);
		double y1B2 = y1A4 + 0.01 * (y2A4 - y1A4);
		double y2B2 = y2A4 + 0.01 * (y2A4 - y1A4);
		Layout childB2 = new Layout(layoutB, x1B2, x2B2, y1B2, y2B2);

		// child of the 2nd child similar (or not) to the 2nd child of the 4th child of the containing layout A
		double x1B21 = x1A42 + 0.01 * (x2A42 - x1A42);
		double x2B21 = x2A42 + 0.01 * (x2A42 - x1A42);
		double y1B21 = y1A42 + 0.01 * (y2A42 - y1A42);
		double y2B21 = y2A42 + 0.01 * (y2A42 - y1A42);
		Layout childB21 = new Layout(childB2, x1B21, x2B21, y1B21, y2B21);

		assert !layoutB.belongsToRoot(layoutA, 0.0097);
		assert layoutB.belongsToRoot(layoutA, 0.0103);

		// adding A2 a child similar to Layout B
		Layout childA21 = new Layout(childA2, 0.1, 0.9, 0.1, 0.9);
		Layout childA211 = new Layout(childA21, x1B1, x2B1, y1B1, y2B1);
		Layout childA212 = new Layout(childA21, x1B2, x2B2, y1B2, y2B2);
		Layout childA2121 = new Layout(childA212, x1B21, x2B21, y1B21, y2B21);

		List<Layout> containingDesc = layoutB.belongsToDesc(layoutA, 0.011, new ArrayList<Layout>());
		assert containingDesc.get(0) == layoutA;
		assert containingDesc.get(1) == childA21;

	}

}
