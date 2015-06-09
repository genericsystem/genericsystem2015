package org.genericsystem.kernel;

import org.genericsystem.defaults.exceptions.InstanceValueClassConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class InstanceValueClassConstraintTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");

		assert myCar.getInstanceValueClassConstraint() == null;
		car.setInstanceValueClassConstraint(String.class);
		assert String.class.equals(car.getInstanceValueClassConstraint());
		car.setInstanceValueClassConstraint(null);
		myCar.updateValue(null);

		assert car.getInstanceValueClassConstraint() == null;

	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.setInstanceValueClassConstraint(Integer.class);

		myCar.addHolder(power, 125);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.setInstanceValueClassConstraint(Integer.class);

		catchAndCheckCause(() -> myCar.addHolder(power, "125"), InstanceValueClassConstraintViolationException.class);

	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.setInstanceValueClassConstraint(Integer.class);

		myCar.addHolder(power, 125);
		power.setInstanceValueClassConstraint(null);
		myCar.addHolder(power, "230");
	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		power.setInstanceValueClassConstraint(Integer.class);

		myCar.addHolder(power, 125);
		power.setInstanceValueClassConstraint(null);
		myCar.addHolder(power, "230");
	}
}
