package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.exceptions.InstanceValueClassConstraintViolationException;
import org.genericsystem.defaults.exceptions.PropertyConstraintViolationException;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;
import org.genericsystem.defaults.exceptions.UniqueValueConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class ConsitencyConstraintTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addAttribute("carColor", color);

		myCar.addHolder(carColor, "myCarRed", red);
		myCar.addHolder(carColor, "myCarYellow", yellow);
		catchAndCheckCause(() -> carColor.enableSingularConstraint(ApiStatics.BASE_POSITION), SingularConstraintViolationException.class);
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myFirstCar = car.addInstance("myFirstCar");
		Generic mySecondCar = car.addInstance("mySecondCar");
		Generic power = root.addInstance("Power");
		Generic powerAttribute = car.addAttribute(power, "Power");
		myFirstCar.addHolder(power, 125);
		mySecondCar.addHolder(power, 125);
		catchAndCheckCause(() -> powerAttribute.enableUniqueValueConstraint(), UniqueValueConstraintViolationException.class);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		Generic myCar = car.addInstance("myCar");
		myCar.addHolder(power, "126");
		myCar.addHolder(power, "123");
		catchAndCheckCause(() -> power.enablePropertyConstraint(), PropertyConstraintViolationException.class);
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic power = root.addInstance("Power");
		car.addAttribute(power, "Power");
		myCar.addHolder(power, "125");

		catchAndCheckCause(() -> power.setInstanceValueClassConstraint(Integer.class), InstanceValueClassConstraintViolationException.class);

	}
}
