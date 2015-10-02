package org.genericsystem.kernel;

import java.util.Arrays;
import org.genericsystem.api.core.exceptions.CrossEnginesAssignementsException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class MultipleRootsTest extends AbstractTest {

	public void test001() {
		LightServerEngine root1 = new LightServerEngine();
		String nameOfsecondRoot = "SecondRoot";
		LightServerEngine root2 = new LightServerEngine(nameOfsecondRoot);
		assert root1.getMeta().equals(root1);
		assert root1.getSupers().isEmpty();
		assert root1.getComponents().isEmpty();
		assert Statics.ENGINE_VALUE.equals(root1.getValue());
		assert root1.isAlive();
		assert !root2.getMeta().equals(root1);
		assert root2.getMeta().equals(root2);
		assert root2.getSupers().isEmpty();
		assert root2.getComponents().isEmpty();
		assert root2.getValue().equals(nameOfsecondRoot);
		assert root2.isAlive();
	}

	public void test002() {
		LightServerEngine root1 = new LightServerEngine();
		LightServerEngine root2 = new LightServerEngine("SecondRoot");
		Generic car1 = root1.addInstance("Car");
		Generic car2 = root2.addInstance("Car");
		catchAndCheckCause(() -> root1.addInstance("Power", car1, car2), CrossEnginesAssignementsException.class);
	}

	public void test003() {
		LightServerEngine root1 = new LightServerEngine();
		LightServerEngine root2 = new LightServerEngine("SecondRoot");
		Generic car = root1.addInstance("Car");
		root2.addInstance("Car");
		catchAndCheckCause(() -> root2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test004() {
		LightServerEngine root1 = new LightServerEngine("FirstRoot");
		LightServerEngine root2 = new LightServerEngine("SecondRoot");
		Generic car = root1.addInstance("Car1");
		root2.addInstance("Car2");
		catchAndCheckCause(() -> root2.addInstance("Power", car), CrossEnginesAssignementsException.class);
	}

	public void test005() {
		LightServerEngine root1 = new LightServerEngine();
		LightServerEngine root2 = new LightServerEngine("SecondRoot");
		Generic car = root2.addInstance("Car");
		Generic robot = root2.addInstance("Robot");
		catchAndCheckCause(() -> root1.addInstance(Arrays.asList(car, robot), "Transformer"), CrossEnginesAssignementsException.class);
	}
}
