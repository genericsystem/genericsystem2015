package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class CascadeRemoveTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.BASE_POSITION);
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		Generic myCarRed = myCar.addLink(carColor, "defaultColor", red);
		myCarRed.remove();

		assert red.isAlive();
		assert !myCar.isAlive();
		assert !myCarRed.isAlive();
		assert !myCarRed.getBaseComponent().isAlive();
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.TARGET_POSITION);
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		Generic myCarRed = myCar.addLink(carColor, "defaultColor", red);
		myCarRed.remove();
		assert myCar.isAlive();
		assert !red.isAlive();
		assert !myCarRed.isAlive();
		assert !myCarRed.getTargetComponent().isAlive();
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.BASE_POSITION);
		Generic myCar = car.addInstance("myCar");
		Generic myCarColor = myCar.addLink(carColor, "defaultColor", color);
		myCarColor.remove();
		assert color.isAlive();
		assert !myCar.isAlive();
		assert !myCarColor.isAlive();
		assert !myCarColor.getBaseComponent().isAlive();
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.TARGET_POSITION);
		Generic red = color.addInstance("red");
		Generic carRed = car.addLink(carColor, "defaultColor", red);
		carRed.remove();
		assert car.isAlive();
		assert !red.isAlive();
		assert !carRed.isAlive();
		assert !carRed.getTargetComponent().isAlive();
	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.BASE_POSITION);
		Generic red = color.addInstance("red");
		Generic carRed = car.addLink(carColor, "defaultColor", red);
		catchAndCheckCause(() -> carRed.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test006() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableCascadeRemove(ApiStatics.TARGET_POSITION);
		Generic myCar = car.addInstance("myCar");
		Generic myCarColor = myCar.addLink(carColor, "defaultColor", color);
		assert myCar.isAlive();
		assert myCarColor.isAlive();
		assert myCarColor.getTargetComponent().isAlive();
		catchAndCheckCause(() -> myCarColor.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

}
