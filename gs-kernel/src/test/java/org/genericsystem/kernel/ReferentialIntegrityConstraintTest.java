package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ReferentialIntegrityConstraintTest extends AbstractTest {

	public void test001() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		car.remove();
	}

	public void test003() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		car.remove();
	}

	public void test004() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005() {
		Engine root = new Engine();
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
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaRelation().enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test007() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		myCar.remove();
	}

	public void test008() {
		Engine root = new Engine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test009() {
		Engine root = new Engine();
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
