package org.genericsystem.kernel;

import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.testng.annotations.Test;

@Test
public class CornerCaseTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = vehicle.addAttribute("Power");
		car.addAttribute(power, "FirstCarPower");
		car.addAttribute(power, "SecondCarPower");
		Generic myCar = car.addInstance("myCar");
		catchAndCheckCause(() -> myCar.addHolder(power, 233), AmbiguousSelectionException.class);
	}

	public void test002() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = vehicle.addAttribute("Power");
		Generic carPower = car.addAttribute(power, "CarPower");
		Generic myCar = car.addInstance("myCar");
		Generic v233 = myCar.addHolder(power, 233);
		assert v233.isInstanceOf(carPower) : v233.info();
		Generic v233_2 = myCar.setHolder(power, 233);
		assert v233 == v233_2;
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		assert root.getInstance("Power", car) == null;
		assert root.getMetaAttribute().getInstance("Power", car).equals(power);
	}

}
