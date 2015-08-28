package org.genericsystem.cache;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveTest extends AbstractClassicTest {

	public void test002_IterateAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		car.addInstance("myCar1");
		car.addInstance("myCar2");
		ClientGeneric myCar3 = car.addInstance("myCar3");
		car.addInstance("myCar4");

		Snapshot<ClientGeneric> myCars = car.getInstances();

		Iterator<ClientGeneric> iterator = myCars.iterator();
		int cpt = 0;
		myCar3.remove();
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3;
	}

	public void test004_IterateAndRemoveInLoop_beforeFindIt() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		car.addInstance("myCar2");
		car.addInstance("myCar3");
		car.addInstance("myCar4");

		for (ClientGeneric v : car.getInstances())
			if (v.equals(myCar1))
				v.remove();
		assert car.getInstances().size() == 3;
	}

	public void test005_IterateAndRemoveInLoop_beforeFindIt() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		car.addInstance("myCar1");
		car.addInstance("myCar2");
		ClientGeneric myCar3 = car.addInstance("myCar3");
		car.addInstance("myCar4");

		Snapshot<ClientGeneric> myCars = car.getInstances();

		Iterator<ClientGeneric> iterator = myCars.iterator();
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
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric doors = car.addAttribute("Doors");

		for (ClientGeneric v : car.getComponents())
			v.remove();
		assert car.getComponents().size() == 0;
	}

	public void test007_IterateAndRemoveInLoop_attributes() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric doors = car.addAttribute("Doors");

		for (ClientGeneric v : car.getComponents()) {
			color.remove();
			power.remove();
			doors.remove();
		}
		assert car.getComponents().size() == 0;
	}
}
