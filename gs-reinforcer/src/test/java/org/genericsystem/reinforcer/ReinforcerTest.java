package org.genericsystem.reinforcer;

import org.testng.annotations.Test;

@Test
public class ReinforcerTest {

	// 4 labels at the same places in 4 all images, 2 classes depending on text.
	public void test001() {
		Reinforcer reinforcer = new Reinforcer();
		Labels labels1 = new Labels();
		labels1.addLabel(1, 1, 50, 20, "Surname;");
		labels1.addLabel(55, 1, 100, 20, "Kenobi");
		labels1.addLabel(1, 22, 50, 42, "Name:");
		labels1.addLabel(55, 22, 100, 42, "Obi-Wan");
		reinforcer.createNewTemplate(labels1, "horizSep");
		Labels labels2 = new Labels();
		labels2.addLabel(1, 1, 50, 20, "Surname;");
		labels2.addLabel(55, 1, 100, 20, "Vader");
		labels2.addLabel(1, 22, 50, 42, "Name:");
		labels2.addLabel(55, 22, 100, 42, "Darth");
		reinforcer.reinforce(labels2, "horizSep");
		Labels labels3 = new Labels();
		labels3.addLabel(1, 1, 50, 20, "Surname;");
		labels3.addLabel(55, 1, 100, 20, "Name:");
		labels3.addLabel(1, 22, 50, 42, "Kenobi");
		labels3.addLabel(55, 22, 100, 42, "Obi-Wan");
		reinforcer.createNewTemplate(labels3, "vertSep");
		Labels labels4 = new Labels();
		labels4.addLabel(1, 1, 50, 20, "Surname;");
		labels4.addLabel(55, 1, 100, 20, "Name:");
		labels4.addLabel(1, 22, 50, 42, "Vader");
		labels4.addLabel(55, 22, 100, 42, "Darth");
		reinforcer.reinforce(labels4, "vertSep");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 50, 20, "Surname;");
		labels5.addLabel(55, 1, 100, 20, "Name:");
		labels5.addLabel(1, 22, 50, 42, "Skywalker");
		labels5.addLabel(55, 22, 100, 42, "Luke");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 50, 20, "Surname;");
		labels6.addLabel(55, 1, 100, 20, "Name:");
		labels6.addLabel(1, 22, 50, 42, "Yoda");
		labels6.addLabel(55, 22, 100, 42, "Master");
		Labels labels7 = new Labels();
		labels7.addLabel(1, 1, 50, 20, "Surname;");
		labels7.addLabel(55, 1, 100, 20, "Skywalker");
		labels7.addLabel(1, 22, 50, 42, "Name:");
		labels7.addLabel(55, 22, 100, 42, "Luke");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 50, 20, "Surname;");
		labels8.addLabel(55, 1, 100, 20, "Yoda");
		labels8.addLabel(1, 22, 50, 42, "Name:");
		labels8.addLabel(55, 22, 100, 42, "Master");

		// Test:
		assert "horizSep".equals(reinforcer.getBestTemplate(labels1));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels2));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels3));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels4));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels5));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels6));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels7));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels8));
	}

	// 4 labels at roughly the same places in all images, 2 classes depending on text.
	public void test002() {
		Reinforcer reinforcer = new Reinforcer();
		Labels labels1 = new Labels();
		labels1.addLabel(5, 2, 50, 21, "Surname;");
		labels1.addLabel(60, 3, 110, 19, "Kenobi");
		labels1.addLabel(6, 24, 54, 44, "Name.");
		labels1.addLabel(60, 23, 104, 46, "Obi-Wan");
		reinforcer.createNewTemplate(labels1, "horizSep");
		Labels labels2 = new Labels();
		labels2.addLabel(5, 3, 106, 39, "Surnarne:");
		labels2.addLabel(114, 4, 207, 41, "Vader");
		labels2.addLabel(6, 45, 108, 83, "Name:");
		labels2.addLabel(117, 43, 210, 85, "Darth");
		reinforcer.reinforce(labels2, "horizSep");
		Labels labels3 = new Labels();
		labels3.addLabel(6, 1, 78, 27, "Surname;");
		labels3.addLabel(84, 6, 144, 33, "Name:");
		labels3.addLabel(2, 33, 81, 60, "Kenobi");
		labels3.addLabel(84, 36, 151, 69, "Obi-Wan");
		reinforcer.createNewTemplate(labels3, "vertSep");
		Labels labels4 = new Labels();
		labels4.addLabel(20, 42, 69, 59, "Surname;");
		labels4.addLabel(76, 43, 121, 60, "Name:");
		labels4.addLabel(21, 62, 72, 83, "Vader");
		labels4.addLabel(74, 65, 123, 81, "Darth");
		reinforcer.reinforce(labels4, "vertSep");

		Labels labels5 = new Labels();
		labels5.addLabel(1, 1, 50, 20, "Surname;");
		labels5.addLabel(55, 1, 100, 20, "Name:");
		labels5.addLabel(1, 22, 50, 42, "Skywalker");
		labels5.addLabel(55, 22, 100, 42, "Luke");
		Labels labels6 = new Labels();
		labels6.addLabel(1, 1, 50, 20, "Surnarne;");
		labels6.addLabel(55, 1, 100, 20, "Name:");
		labels6.addLabel(1, 22, 50, 42, "Yoda");
		labels6.addLabel(55, 22, 100, 42, "Master");
		Labels labels7 = new Labels();
		labels7.addLabel(1, 5, 50, 30, "Sumame;");
		labels7.addLabel(55, 7, 100, 29, "Skywalker");
		labels7.addLabel(1, 30, 50, 50, "Narne:");
		labels7.addLabel(55, 31, 100, 49, "Luke");
		Labels labels8 = new Labels();
		labels8.addLabel(1, 1, 50, 20, "Surname;");
		labels8.addLabel(55, 1, 100, 20, "Yoda");
		labels8.addLabel(1, 22, 50, 42, "Name:");
		labels8.addLabel(55, 22, 100, 42, "Master");

		// Test:
		assert "horizSep".equals(reinforcer.getBestTemplate(labels1));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels2));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels3));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels4));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels5));
		assert "vertSep".equals(reinforcer.getBestTemplate(labels6));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels7));
		assert "horizSep".equals(reinforcer.getBestTemplate(labels8));
	}
}
