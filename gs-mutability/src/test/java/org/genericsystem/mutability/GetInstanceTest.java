package org.genericsystem.mutability;

import java.util.Collections;

import org.genericsystem.kernel.Generic;
import org.genericsystem.kernel.Root;
import org.testng.annotations.Test;

@Test
public class GetInstanceTest extends AbstractTest {

	public void test001_getInstance() {
		Root engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");

		assert vehicle.getInstance("myBmw") == null;
		assert car.getInstance("myBmw") == myBmw;
	}

	public void test002_getInstance() {
		Root engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = engine.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw115 = vehiclePower.addInstance(115, myBmw);

		assert vehiclePower.getInstance(115, myBmw) == myBmw115;
	}

	public void test003_getInstance() {
		Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic vehicle = engine.addInstance("Vehicle");
		Generic carVehicle = engine.addInstance(vehicle, "Car");

		assert engine.getInstance(Collections.emptyList(), "Car") == car;
		assert engine.getInstance(vehicle, "Car") == carVehicle;
	}

	public void test004_getInstance() {
		Root engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myBmw = vehicle.addInstance("myBmw");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic myBmwCar = car.addInstance("myBmw");

		assert vehicle.getInstance("myBmw") == myBmw;
		assert car.getInstance("myBmw") == myBmwCar;
	}
}
