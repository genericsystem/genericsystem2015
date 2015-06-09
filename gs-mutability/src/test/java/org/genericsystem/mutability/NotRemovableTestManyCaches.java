package org.genericsystem.mutability;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class NotRemovableTestManyCaches extends AbstractTest {

	public void test001_aliveEx() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

}
