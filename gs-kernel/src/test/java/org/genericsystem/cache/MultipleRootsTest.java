package org.genericsystem.cache;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.CrossEnginesAssignementsException;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class MultipleRootsTest extends AbstractTest {

	public void test001_Engine_name() {
		Engine engine1 = new Engine();
		String nameOfsecondEngine = "SecondEngine";
		Engine engine2 = new Engine(nameOfsecondEngine);
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
		Engine engine1 = new Engine();
		Engine engine2 = new Engine("SecondEngine");
		engine1.addInstance("Car");
		Generic car = engine2.addInstance("Car");
		catchAndCheckCause(() -> engine1.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test003_addInstance_attribute() {
		Engine engine1 = new Engine();
		Engine engine2 = new Engine("SecondEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test004_addInstance_attribute() {
		Engine engine1 = new Engine("FirstEngine");
		Engine engine2 = new Engine("SecondEngine");
		Generic car = engine1.addInstance("Car");
		engine2.addInstance("Car");
		catchAndCheckCause(() -> engine2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test005_addInstance_overrides() {
		Engine engine1 = new Engine();
		Engine engine2 = new Engine("SecondEngine");
		Generic car = engine2.addInstance("Car");
		Generic robot = engine2.addInstance("Robot");
		catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), CrossEnginesAssignementsException.class);
		// catchAndCheckCause(() -> engine1.addInstance(Arrays.asList(car, robot), "Transformer"), IllegalStateException.class);
	}

}
