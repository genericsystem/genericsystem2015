package org.genericsystem.distributed.cacheonserver;

import java.util.Arrays;
import org.genericsystem.api.core.exceptions.CrossEnginesAssignementsException;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.HeavyServerEngine;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class MultipleRootsTest extends AbstractTest {

	public void test001_ServerEngine_name() {
		HeavyServerEngine engine1 = new HeavyServerEngine();
		String nameOfsecondServerEngine = "SecondServerEngine";
		HeavyServerEngine engine2 = new HeavyServerEngine(nameOfsecondServerEngine);
		assert engine1.getMeta().equals(engine1);
		assert engine1.getSupers().isEmpty();
		assert engine1.getComponents().size() == 0;
		assert Statics.ENGINE_VALUE.equals(engine1.getValue());
		assert engine1.isAlive();
		assert engine2.getMeta().equals(engine2);
		assert engine2.getSupers().size() == 0;
		assert engine2.getComponents().size() == 0;
		assert engine2.getValue().equals(nameOfsecondServerEngine);
		assert engine2.isAlive();
	}

	public void test002_addInstance_attribute() {
		HeavyServerEngine engine1 = new HeavyServerEngine();
		HeavyServerEngine engine2 = new HeavyServerEngine("SecondServerEngine");
		engine1.addInstance("Car");
		Generic car = engine2.addInstance("Car");
		catchAndCheckCause(() -> engine1.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test003_addInstance_attribute() {
		HeavyServerEngine engine1 = new HeavyServerEngine();
		HeavyServerEngine engine2 = new HeavyServerEngine("SecondServerEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test004_addInstance_attribute() {
		HeavyServerEngine engine1 = new HeavyServerEngine("FirstServerEngine");
		HeavyServerEngine engine2 = new HeavyServerEngine("SecondServerEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test005_addInstance_overrides() {
		HeavyServerEngine engine1 = new HeavyServerEngine();
		HeavyServerEngine engine2 = new HeavyServerEngine("SecondServerEngine");
		Generic car = engine2.addInstance("Car");
		Generic robot = engine2.addInstance("Robot");
		catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), CrossEnginesAssignementsException.class);
		// catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), IllegalStateException.class);
	}

}
