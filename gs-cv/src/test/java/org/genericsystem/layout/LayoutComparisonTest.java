package org.genericsystem.layout;

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

		// Containing Layout
		Layout layout1 = new Layout(null, Math.random(), Math.random(), Math.random(), Math.random());

		// 1st child
		double x111 = 0.1;
		double x211 = 0.6;
		double y111 = 0.1;
		double y211 = 0.3;
		Layout child11 = new Layout(layout1, x111, x211, y111, y211);

		// 2nd child
		double x121 = 0.7;
		double x221 = 0.9;
		double y121 = 0.1;
		double y221 = 0.3;
		Layout child21 = new Layout(layout1, x121, x221, y121, y221);

		// 3rd child
		double x131 = 0.1;
		double x231 = 0.3;
		double y131 = 0.4;
		double y231 = 0.9;
		Layout child31 = new Layout(layout1, x131, x231, y131, y231);

		// 4th child
		double x141 = 0.4;
		double x241 = 0.9;
		double y141 = 0.4;
		double y241 = 0.9;
		Layout child41 = new Layout(layout1, x141, x241, y141, y241);

		// Contained Layout
		Layout layout2 = new Layout(null, Math.random(), Math.random(), Math.random(), Math.random());

		// 1st child similar (or not) to the 1st child of the containing layout
		double x112 = x111 + 0.01 * (x211 - x111);
		double x212 = x211 + 0.01 * (x211 - x111);
		double y112 = y111 + 0.01 * (y211 - y111);
		double y212 = y211 - 0.01 * (y211 - y111);
		Layout child12 = new Layout(layout2, x112, x212, y112, y212);

		// 2nd child similar (or not) to the 4th child of the containing layout
		double x122 = x141 + 0.01 * (x241 - x141);
		double x222 = x241 + 0.01 * (x241 - x141);
		double y122 = y141 + 0.01 * (y241 - y141);
		double y222 = y241 + 0.01 * (y241 - y141);
		Layout child22 = new Layout(layout2, x122, x222, y122, y222);

		List<Layout> counterparts = layout1.childrenCounterparts(layout2, 0.009);
		assert counterparts == null;
		counterparts = layout1.childrenCounterparts(layout2, 0.011);
		assert counterparts.get(0) == child11 && counterparts.get(1) == child41;

	}

}
