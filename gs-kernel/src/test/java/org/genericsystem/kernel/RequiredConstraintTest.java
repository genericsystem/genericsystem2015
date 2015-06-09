package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.testng.annotations.Test;

@Test
public class RequiredConstraintTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic numberDoors = root.addInstance("NumberDoors");

		Generic power = car.addAttribute("Power");
		Generic carColor = car.setRelation("CarColor", color);
		Generic carNumberDoors = car.setRelation("CarNumberDoors", numberDoors);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		carColor.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		carNumberDoors.enableSingularConstraint(ApiStatics.BASE_POSITION);

		Generic red = color.addInstance("red");
		Generic fiveDoors = numberDoors.addInstance("fiveDoors");
		Generic myBmw = car.addInstance("myBmw");

		car.setLink(carNumberDoors, "carFiveDoors", fiveDoors);
		car.setHolder(power, "123");
		Generic myBmwRed = myBmw.setLink(carColor, "myBmwRed", red);

		myBmw.remove();
		assert !myBmw.isAlive();
		assert !myBmwRed.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert carColor.isAlive();
		assert red.isAlive();
		assert !myBmw.isAlive();
		assert !myBmwRed.isAlive();
	}

}
