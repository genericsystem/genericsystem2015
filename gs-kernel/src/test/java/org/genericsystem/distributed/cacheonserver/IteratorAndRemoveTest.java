package org.genericsystem.distributed.cacheonserver;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.CosClientEngine;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveTest extends AbstractTest {

	public void test002_IterateAndRemove() {
		CosClientEngine engine = new CosClientEngine();
		Generic car = engine.addInstance("Car");
		car.addInstance("myCar1");
		car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		car.addInstance("myCar4");

		Snapshot<Generic> myCars = car.getInstances();

		int cpt = 0;
		myCar3.remove();
		Iterator<Generic> iterator = myCars.iterator();
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3;
	}

	public void test004_IterateAndRemoveInLoop_beforeFindIt() {
		CosClientEngine engine = new CosClientEngine();
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

	// public void test005_IterateAndRemoveInLoop_beforeFindIt() {
	// LightClientEngine engine = new LightClientEngine();
	// Generic car = engine.addInstance("Car");
	// car.addInstance("myCar1");
	// car.addInstance("myCar2");
	// Generic myCar3 = car.addInstance("myCar3");
	// car.addInstance("myCar4");
	//
	// Snapshot<Generic> myCars = car.getInstances();
	//
	// Iterator<Generic> iterator = myCars.iterator(); //iterator is no longer aware!
	// int cpt = 0;
	//
	// while (iterator.hasNext()) {
	// if (cpt == 0)
	// myCar3.remove();
	// if (iterator.next().equals(myCar3))
	// assert false : "Removed Object found";
	// cpt++;
	// }
	// assert cpt == 3 : cpt;
	// assert car.getInstances().size() == 3;
	// }

	public void test006_IterateAndRemoveInLoop_attributes() {
		CosClientEngine engine = new CosClientEngine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic power = car.addAttribute("Power");
		Generic doors = car.addAttribute("Doors");

		for (Generic v : car.getComponents())
			v.remove();
		assert car.getComponents().size() == 0;
	}

	public void test007_IterateAndRemoveInLoop_attributes() {
		CosClientEngine engine = new CosClientEngine();
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
