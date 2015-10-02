package org.genericsystem.lightserverengine;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.LightServerEngine;
import org.testng.annotations.Test;

@Test
public class AliveConstraintTest extends AbstractTest {

	public void test001() {
		LightServerEngine engine = new LightServerEngine();
		Generic car = engine.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("carColor", color);
		myBmw.remove();
		assert !myBmw.isAlive();
		catchAndCheckCause(() -> myBmw.addLink(carColor, "myBmwRed", red), AliveConstraintViolationException.class);
	}

	public void test002() {
		LightServerEngine engine = new LightServerEngine();
		Generic car = engine.addInstance("Car");
		car.remove();
		assert !car.isAlive();
		catchAndCheckCause(() -> car.addInstance("myAudi"), AliveConstraintViolationException.class);
	}

	public void test003() {
		LightServerEngine engine = new LightServerEngine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		power.remove();
		assert !power.isAlive();
		assert car.isAlive();
		catchAndCheckCause(() -> car.addHolder(power, 235), AliveConstraintViolationException.class);
	}

	public void test004() {
		LightServerEngine engine = new LightServerEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		car.remove();
		assert !car.isAlive();
		assert vehicle.isAlive();
	}

	public void test005() {
		LightServerEngine engine = new LightServerEngine();
		Generic car = engine.addInstance("Car");
		Generic airConditioner = engine.addInstance("airConditioner", car);
		Generic radio = engine.addInstance("Radio", car);
		assert car.isAlive();
		assert airConditioner.isAlive();
		assert radio.isAlive();
		airConditioner.remove();
		assert car.isAlive();
		assert !airConditioner.isAlive();
		assert radio.isAlive();
		car.remove();
		assert !car.isAlive();
		assert !radio.isAlive();
	}

	public void test006() {
		Generic root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		vehicle.remove();
		catchAndCheckCause(() -> root.addInstance(vehicle, "Car"), AliveConstraintViolationException.class);
	}

}
