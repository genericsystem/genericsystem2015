package org.genericsystem.kernel;

import org.genericsystem.defaults.exceptions.UniqueValueConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UniqueValueConstraintTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic power = root.addInstance("Power", car);
		myBmw.addHolder(power, "125");

		assert !car.isUniqueValueEnabled();
		car.enableUniqueValueConstraint();
		assert car.isUniqueValueEnabled();
		car.disableUniqueValueConstraint();
		assert !car.isUniqueValueEnabled();

	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.enableUniqueValueConstraint();
		myBmw.addHolder(power, 125);
		catchAndCheckCause(() -> myAudi.addHolder(power, 125), UniqueValueConstraintViolationException.class);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.enableUniqueValueConstraint();
		myBmw.addHolder(power, 125);
		myAudi.addHolder(power, "125");
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.enableUniqueValueConstraint();
		myBmw.addHolder(power, 125);
		power.disableUniqueValueConstraint();
		myAudi.addHolder(power, 125);
	}
}
