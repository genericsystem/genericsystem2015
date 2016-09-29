package org.genericsystem.kernel;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;

public class DirectClassTest extends AbstractTest {
	public void test001() {
		Engine engine = new Engine(DirectVehicle.class);
		assert engine.find(DirectVehicle.class) instanceof DirectVehicle;

	}

	public void test002() {
		Engine engine = new Engine(Vehicle.class);
		assert engine.find(Vehicle.class) instanceof Generic;

	}

	@DirectClass
	@SystemGeneric
	public static interface DirectVehicle {

	}

	@SystemGeneric
	public static interface Vehicle {

	}
}
