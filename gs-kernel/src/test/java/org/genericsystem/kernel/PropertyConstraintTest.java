package org.genericsystem.kernel;

import java.util.Arrays;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.exceptions.PropertyConstraintViolationException;
import org.genericsystem.kernel.BasicEngine;
import org.testng.annotations.Test;

@Test
public class PropertyConstraintTest extends AbstractTest {

	public void test001() {
		BasicEngine root = new BasicEngine();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		Generic myCar = car.addInstance("myCar");

		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();

		Generic myCar123 = myCar.addHolder(power, "123");
		assert !myCar123.inheritsFrom(power, "126", Arrays.asList(myCar));

		catchAndCheckCause(() -> myCar.addHolder(power, "126"), PropertyConstraintViolationException.class);
	}

	public void test002() {
		BasicEngine root = new BasicEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic carColor = car.addAttribute("CarColor", color);

		carColor.enablePropertyConstraint();
		assert carColor.isPropertyConstraintEnabled();
		myCar.addLink(carColor, "myCarRed", red);
		myCar.addLink(carColor, "myCarBlue", blue);
		assert myCar.getHolders(carColor).size() == 2;
		catchAndCheckCause(() -> myCar.addLink(carColor, "mySecondCarRed", red), PropertyConstraintViolationException.class);
	}

	public void test003() {
		BasicEngine root = new BasicEngine();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		Generic myCar = car.addInstance("myCar");

		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();
		myCar.addHolder(power, "123");
		catchAndCheckCause(() -> myCar.addHolder(power, "126"), PropertyConstraintViolationException.class);
	}

	public void test004() {
		BasicEngine Root = new BasicEngine();
		Generic car = Root.addInstance("Car");
		Generic power = Root.addInstance("Power", car);
		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();
		power.setInstance("123", car);
		power.setInstance("126", car);
		assert power.getInstances().size() == 1;
		power.getInstances().forEach(x -> x.getValue().equals("126"));
	}

	public void test005() {
		BasicEngine root = new BasicEngine();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();
		power.setInstance("123", car);
		power.setInstance("126", car);
		assert power.getInstances().size() == 1;
		power.getInstances().forEach(x -> x.getValue().equals("126"));
		power.disablePropertyConstraint();
		assert !power.isPropertyConstraintEnabled();
		power.setInstance("123", car);
		assert power.getInstances().size() == 2;
	}

}
