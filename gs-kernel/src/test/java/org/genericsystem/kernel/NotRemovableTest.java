package org.genericsystem.kernel;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.PropertyConstraint;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class NotRemovableTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, 123);

		myBmw123.remove();
		catchAndCheckCause(() -> myBmw123.remove(), AliveConstraintViolationException.class);
	}

	public void test002() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		car.addAttribute("Color");
		car.addInstance("myBmw");

		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test003() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		myBmw.addHolder(power, "red");

		catchAndCheckCause(() -> power.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test004() {
		Root root = new Root(Power.class);
		Generic power = root.find(Power.class);
		assert power.isPropertyConstraintEnabled();
		catchAndCheckCause(() -> power.disablePropertyConstraint(), IllegalAccessException.class);
	}

	@SystemGeneric
	public static class Vehicle {

	}

	@SystemGeneric
	@Components(Vehicle.class)
	@PropertyConstraint
	public static class Power {

	}
}
