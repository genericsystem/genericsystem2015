package org.genericsystem.kernel;

import java.util.Iterator;

import org.genericsystem.api.core.Snapshot;
import org.testng.annotations.Test;

@Test
public class IteratorAndRemoveTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		car.addInstance("myFirstCar");
		car.addInstance("mySecondCar");
		Generic myThirdCar = car.addInstance("myThirdCar");
		car.addInstance("myFourthCar");

		Snapshot<Generic> myCars = car.getInstances();

		Iterator<Generic> iterator = myCars.iterator();
		int cpt = 0;
		myThirdCar.remove();
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3;
	}

	public void test002() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic myFirstCar = car.addInstance("myFirstCar");
		car.addInstance("mySecondCar");
		car.addInstance("myThirdCar");
		car.addInstance("myFourthCar");

		for (Generic v : car.getInstances())
			if (v.equals(myFirstCar))
				v.remove();
		assert car.getInstances().size() == 3;
	}

	public void test003() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		car.addInstance("myFirstCar");
		car.addInstance("mySecondCar");
		Generic myCar3 = car.addInstance("myThirdCar");
		car.addInstance("myFourthCar");

		Snapshot<Generic> myCars = car.getInstances();

		Iterator<Generic> iterator = myCars.iterator();
		int cpt = 0;
		while (iterator.hasNext()) {
			if (cpt == 0)
				myCar3.remove();
			if (iterator.next().equals(myCar3))
				assert false : "Remove Object";
			cpt++;
		}
		assert cpt == 3 : cpt;
		assert car.getInstances().size() == 3;
	}

	public void test004() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		car.addAttribute("Options");
		car.addAttribute("Power");
		car.addAttribute("Doors");

		for (Generic v : car.getComponents())
			v.remove();
		assert car.getComponents().size() == 0;
	}

	public void test005() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic options = car.addAttribute("Options");
		Generic power = car.addAttribute("Power");
		Generic doors = car.addAttribute("Doors");

		for (@SuppressWarnings("unused")
		Generic v : car.getComponents()) {
			options.remove();
			power.remove();
			doors.remove();
		}
		assert car.getComponents().size() == 0;
	}
}
