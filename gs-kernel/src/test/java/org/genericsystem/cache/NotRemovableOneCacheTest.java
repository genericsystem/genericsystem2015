package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class NotRemovableOneCacheTest extends AbstractTest {

	public void test001_aliveEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.clear();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);
	}

	public void test002_aliveEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		assert car.isAlive();
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		assert myBmwRed.isAlive();
		cache.flush();
		assert myBmwRed.isAlive();
		myBmwRed.remove();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);
	}

	public void test002_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		cache.flush();
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test003_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}
}
