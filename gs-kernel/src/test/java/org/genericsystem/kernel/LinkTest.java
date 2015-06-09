package org.genericsystem.kernel;

import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.testng.annotations.Test;

@Test
public class LinkTest extends AbstractTest {

	public void test001() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		Generic myCarRed = myCar.addLink(carColor, null, red);
		assert myCarRed.equals(myCar.getLink(carColor));
		assert myCarRed.equals(red.getLink(carColor, myCar));
	}

	public void test002() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		Generic myCarGreen = myCar.setLink(carColor, null, green);
		assert myCarGreen.equals(myCar.getLink(carColor));
	}

	public void test003() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		Generic red = color.addInstance("red");
		myCar.addLink(carColor, null, green);
		myCar.addLink(carColor, null, red);
		catchAndCheckCause(() -> myCar.getLink(carColor), AmbiguousSelectionException.class);
	}

	public void test004() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		myCar.addLink(carColor, "myCarRed", red);
		myCar.addLink(carColor, "myCarRed", blue);
		catchAndCheckCause(() -> myCar.getLink(carColor, "myCarRed"), AmbiguousSelectionException.class);
	}

	public void test005() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic door = root.addInstance("Door");
		Generic carColorDoor = car.addRelation("CarColorDoor", color, door);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		Generic rightDoor = door.addInstance("rightDoor");
		Generic myCarRightDoorGreen = myCar.addLink(carColorDoor, null, green, rightDoor);
		assert myCarRightDoorGreen.equals(myCar.getLink(carColorDoor));
		assert myCarRightDoorGreen.equals(myCar.getLink(carColorDoor, rightDoor, green));
		assert null == myCar.getLink(carColorDoor, green, green) : myCar.getLink(carColorDoor, green, green).info();
	}

	public void test006() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		car.addLink(carColor, null, green);
		Generic myCarGreen = myCar.addLink(carColor, null, green);
		assert myCarGreen.equals(myCar.getLink(carColor));
		assert myCarGreen.equals(green.getLink(carColor));
	}

	public void test007() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		Generic carGreen = car.addLink(carColor, null, green);
		assert carGreen.equals(myCar.getLink(carColor));
	}

	public void test008() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic green = color.addInstance("green");
		Generic myCarGreen = myCar.addLink(carColor, null, green);
		assert myCarGreen.equals(myCar.getLink(carColor, green));
		assert null == myCar.getLink(carColor, green, green);
	}

	public void test009() {
		final Root root = new Root();
		Generic humain = root.addInstance("Human");
		Generic hierarchy = humain.addRelation("Hierarchy", humain);
		Generic nicolas = humain.addInstance("nicolas");
		Generic michael = humain.addInstance("michael");
		Generic nicolasBossOfMichael = nicolas.addLink(hierarchy, "isBossOf", michael);
		assert nicolasBossOfMichael.equals(nicolas.getLink(hierarchy));
		assert nicolasBossOfMichael.equals(nicolas.getLink(hierarchy, michael));
		assert nicolasBossOfMichael.equals(nicolas.getLink(hierarchy));
		assert nicolasBossOfMichael.equals(nicolas.getLink(hierarchy, michael));
		assert null == nicolas.getLink(hierarchy, nicolas);
	}

	public void test010() {
		final Root root = new Root();
		Generic humain = root.addInstance("Human");
		Generic hierarchy = humain.addRelation("Hierarchy", humain);
		Generic nicolas = humain.addInstance("nicolas");
		humain.addInstance("michael");
		assert hierarchy.equals(nicolas.getRelation(humain)) : nicolas.getRelations().info();
		assert hierarchy.equals(nicolas.getRelation(humain));
		assert null == nicolas.getRelation(humain, humain);
	}

	public void test011() {
		final Root root = new Root();
		Generic humain = root.addInstance("Human");
		Generic hierarchy = humain.addRelation("Hierarchy", humain);
		Generic nicolas = humain.addInstance("nicolas");
		Generic michael = humain.addInstance("michael");
		Generic michaelMichael = michael.addLink(hierarchy, "michaelMichael", michael);
		michael.addLink(hierarchy, "michaelNicolas", nicolas);

		assert michael.getLink(hierarchy, michael) == michaelMichael;
	}

}
