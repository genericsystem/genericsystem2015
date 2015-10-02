package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ReferentialIntegrityConstraintTest extends AbstractTest {

	public void test001() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		car.remove();
	}

	public void test003() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		car.remove();
	}

	public void test004() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005() {
		LightServerEngine root = new LightServerEngine();
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
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		car.addRelation("CarColor", color);
		root.getMetaRelation().enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test007() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		myCar.remove();
	}

	public void test008() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		myCar.addLink(carColor, "myCarRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test009() {
		LightServerEngine root = new LightServerEngine();
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
