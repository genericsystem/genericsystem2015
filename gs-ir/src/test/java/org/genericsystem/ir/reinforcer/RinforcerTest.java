package org.genericsystem.ir.reinforcer;

import org.testng.annotations.Test;

@Test
public class RinforcerTest {

	private Reinforcer reinforcer = new Reinforcer();

	public void test001() {
		AbsoluteLabels labels = new AbsoluteLabels();
		labels.addAbsoluteLabel(0, 0, 9, 9, "First Label");
		labels.addAbsoluteLabel(10, 10, 15, 15, "Second Label");
		reinforcer.reinforce(labels);
		assert reinforcer.getUnclassifiable().contains(labels);
		reinforcer.reinforce(labels);
		assert reinforcer.getUnclassifiable().contains(labels);
		reinforcer.reinforce(labels);
		assert reinforcer.getUnclassifiable().contains(labels);

	}

}
