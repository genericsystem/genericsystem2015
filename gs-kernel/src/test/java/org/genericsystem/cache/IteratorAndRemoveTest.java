package org.genericsystem.cache;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveTest extends AbstractTest {

	public void test002_IterateAndRemove() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		car.addInstance("myCar1");
		car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		car.addInstance("myCar4");

		Snapshot<Generic> myCars = car.getInstances();

		Iterator<Generic> iterator = myCars.iterator();
		int cpt = 0;
		myCar3.remove();
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3;
	}

	public void test004_IterateAndRemoveInLoop_beforeFindIt() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		car.addInstance("myCar2");
		car.addInstance("myCar3");
		car.addInstance("myCar4");

		for (Generic v : car.getInstances())
			if (v.equals(myCar1))
				v.remove();
		assert car.getInstances().size() == 3;
	}

	public void test005_IterateAndRemoveInLoop_beforeFindIt() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		car.addInstance("myCar1");
		car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		car.addInstance("myCar4");

		Snapshot<Generic> myCars = car.getInstances();

		Iterator<Generic> iterator = myCars.iterator();
		int cpt = 0;
		while (iterator.hasNext()) {
			if (cpt == 0)
				myCar3.remove();
			if (iterator.next().equals(myCar3))
				assert false : "Remove Object founded";
			cpt++;
		}
		assert cpt == 3 : cpt;
		assert car.getInstances().size() == 3;
	}

	public void test006_IterateAndRemoveInLoop_attributes() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic power = car.addAttribute("Power");
		Generic doors = car.addAttribute("Doors");

		for (Generic v : car.getComponents())
			v.remove();
		assert car.getComponents().size() == 0;
	}

	public void test007_IterateAndRemoveInLoop_attributes() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic power = car.addAttribute("Power");
		Generic doors = car.addAttribute("Doors");

		for (Generic v : car.getComponents()) {
			color.remove();
			power.remove();
			doors.remove();
		}
		assert car.getComponents().size() == 0;
	}
}
