package org.genericsystem.ir.reinforcer;

import org.testng.annotations.Test;

@Test
public class RinforcerTest {

	private Reinforcer reinforcer = new Reinforcer();

	public void test001() {
		Labels labels = new Labels();
		labels.addLabel(0, 0, 5, 5, "First Label");
		labels.addLabel(10, 10, 15, 15, "Second Label");
		reinforcer.reinforce(labels);
		assert reinforcer.getUnclassifiable().contains(labels);
		reinforcer.reinforce(labels);
		assert reinforcer.getUnclassifiable().contains(labels);
		reinforcer.reinforce(labels);

		System.out.println(labels.normalizeLabels());

		assert reinforcer.getUnclassifiable().contains(labels);

	}

}
