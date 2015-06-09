package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class ReferentialIntegrityConstraintTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		car.remove();
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		car.remove();
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		carColor.enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> myCar.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test006() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaRelation().enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test007() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		myCar.remove();
	}

	public void test008() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test009() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		root.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		myCar.remove();
	}
}
