package org.genericsystem.distributed;

import java.util.Arrays;
import org.genericsystem.api.core.exceptions.CrossEnginesAssignementsException;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.HeavyClientEngine;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class MultipleRootsTest extends AbstractMultipleBasesTest {

	public void test001_Engine_name() {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		String nameOfsecondEngine = "SecondEngine";
		HeavyClientEngine engine2 = new HeavyClientEngine(nameOfsecondEngine);
		assert engine1.getMeta().equals(engine1);
		assert engine1.getSupers().isEmpty();
		assert engine1.getComponents().size() == 0;
		assert Statics.ENGINE_VALUE.equals(engine1.getValue());
		assert engine1.isAlive();
		assert engine2.getMeta().equals(engine2);
		assert engine2.getSupers().size() == 0;
		assert engine2.getComponents().size() == 0;
		assert engine2.getValue().equals(nameOfsecondEngine);
		assert engine2.isAlive();
	}

	public void test002_addInstance_attribute() {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine("SecondEngine");
		engine1.addInstance("Car");
		Generic car = engine2.addInstance("Car");
		catchAndCheckCause(() -> engine1.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test003_addInstance_attribute() {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine("SecondEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test004_addInstance_attribute() {
		HeavyClientEngine engine1 = new HeavyClientEngine("FirstEngine");
		HeavyClientEngine engine2 = new HeavyClientEngine("SecondEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test005_addInstance_overrides() {
		HeavyClientEngine engine1 = new HeavyClientEngine();
		HeavyClientEngine engine2 = new HeavyClientEngine("SecondEngine");
		Generic car = engine2.addInstance("Car");
		Generic robot = engine2.addInstance("Robot");
		catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), CrossEnginesAssignementsException.class);
		// catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), IllegalStateException.class);
	}

}
