package org.genericsystem.reinforcer;

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

	public void test002() {
		Labels labels1 = new Labels();
		labels1.addLabel(1, 2, 29, 32, "Name;");
		labels1.addLabel(33, 2, 83, 31, "Kenobi");
		reinforcer.reinforce(labels1);
		Labels labels2 = new Labels();
		labels2.addLabel(3, 2, 72, 68, "Name:");
		labels2.addLabel(75, 4, 160, 70, "Vader");
		reinforcer.reinforce(labels2);
		Labels labels3 = new Labels();
		labels3.addLabel(3, 2, 72, 68, "Name.");
		labels3.addLabel(75, 4, 262, 70, "Vader-Kenobi");
		reinforcer.reinforce(labels3);

	}
}
