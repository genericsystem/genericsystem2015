package org.genericsystem.cache;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;
import org.genericsystem.kernel.Engine;
import org.testng.annotations.Test;

@Test
public class WeakEquivTest extends AbstractTest {

	// public void test001_weakEquiv_Relation_SingularConstraint() {
	// ServerEngine engine = new ServerEngine();
	// Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	// Generic carColor = engine.addInstance("CarColor", car, color);
	// carColor.enableSingularConstraint(0);
	// Generic myBmw = car.addInstance("myBmw");
	// Generic green = color.addInstance("green");
	// Generic myBmwGreen = carColor.addInstance("myBmwGreen", myBmw, green);
	// Generic yellow = color.addInstance("yellow");
	// assert !myBmwGreen.equiv(carColor, "myBmwYellow", Arrays.asList(myBmw, yellow));
	// }

	// public void test002_weakEquiv_Relation_SingularConstraintAndReferencialIntegrity() {
	// ServerEngine engine = new ServerEngine();
	// Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	// Generic carColor = engine.addInstance("CarColor", car, color);
	// carColor.enableSingularConstraint(0);
	// carColor.enableReferentialIntegrity(0);
	// Generic myBmw = car.addInstance("myBmw");
	// Generic green = color.addInstance("green");
	// Generic yellow = color.addInstance("yellow");
	// Generic myBmwGreen = carColor.addInstance("myBmwGreen", myBmw, green);
	// assert myBmwGreen.equiv(carColor, "myBmwYellow", Arrays.asList(myBmw, yellow));
	// }

	// public void test003_weakEquiv_Relation_SingularConstraintAndReferencialIntegrity_axeOne() {
	// ServerEngine engine = new ServerEngine();
	// Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	// Generic carColor = engine.addInstance("CarColor", car, color);
	// carColor.enableSingularConstraint(1);
	// carColor.enableReferentialIntegrity(1);
	// Generic myBmw = car.addInstance("myBmw");
	// Generic green = color.addInstance("green");
	// Generic yellow = color.addInstance("yellow");
	// Generic myBmwGreen = carColor.addInstance("myBmwGreen", myBmw, green);
	// assert !myBmwGreen.equiv(carColor, "myBmwYellow", Arrays.asList(myBmw, yellow));
	// }

	public void test004_weakEquiv_Relation_SingularConstraintAndReferencialIntegrity_supers() {
		Engine engine = new Engine();
		assert engine.getCurrentCache() != null;
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic carColor = engine.addInstance("CarColor", car, color);
		carColor.enableSingularConstraint(1);
		// carColor.enableReferentialIntegrity(1);
		Generic myBmw = car.addInstance("myBmw");
		Generic green = color.addInstance("green");
		Generic yellow = color.addInstance("yellow");
		Generic myBmwGreen = myBmw.addHolder(carColor, "myBmwGreen", green);
		Generic myBmwGreen2 = carColor.addInstance(myBmwGreen, "myBmwGreen2", myBmw, green);
	}

	public void test005_weakEquiv_Relation_SingularConstraintAndReferencialIntegrity_setInstance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic carColor = engine.addInstance("CarColor", car, color);
		carColor.enableSingularConstraint(1);
		carColor.enableReferentialIntegrity(1);
		Generic myBmw = car.addInstance("myBmw");
		Generic green = color.addInstance("green");
		Generic yellow = color.addInstance("yellow");
		Generic myBmwGreen = carColor.addInstance("myBmwGreen", myBmw, green);

		catchAndCheckCause(() -> carColor.setInstance("myBmwGreen2", myBmw, green), SingularConstraintViolationException.class);
	}

	// public void test006_weakEquiv_DefaultValue() {
	// ServerEngine engine = new ServerEngine();
	// Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	// Generic defaultColor = color.addInstance("Red", car);
	// Generic carColor = engine.addInstance("CarColor", car, color);
	// carColor.enableSingularConstraint(0);
	// carColor.enableReferentialIntegrity(0);
	// Generic myBmw = engine.addInstance(car, "myBmw");
	// Generic green = color.addInstance("green");
	// Generic myBmwGreen = carColor.addInstance("myBmwGreen", myBmw, green);
	// assert myBmwGreen.equiv(carColor, "myBmwYellow", Arrays.asList(myBmw, defaultColor));
	// }
}
