package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class ObservableDependenciesTest extends AbstractTest {

	public void test1() throws InterruptedException {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("MyCar");
		Generic myCar1 = car.addInstance("myCar1");
		Generic myCar2 = car.addInstance("myCar2");
		Generic myCar3 = car.addInstance("myCar3");
		Generic myCar4 = car.addInstance("myCar4");
		assert engine.getCurrentCache().getObservableDependencies(car).size() == 5;
	}
}
