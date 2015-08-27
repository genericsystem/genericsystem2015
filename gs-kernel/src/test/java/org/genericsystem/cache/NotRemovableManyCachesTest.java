package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class NotRemovableManyCachesTest extends AbstractClassicTest {

	public void test001_aliveEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

	public void test003_aliveEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		ClientGeneric car2 = engine.addInstance("Car2");
		ClientGeneric myBmw2 = car2.addInstance("myBmw2");
		catchAndCheckCause(() -> myBmw2.addHolder(color, "red2"), MetaRuleConstraintViolationException.class);
	}

	public void test001_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		cache.flush();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientCache cache2 = engine.newCache().start();
		ClientCache cache3 = engine.newCache().start();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		cache3.flush();
		cache2.start();
		cache2.shiftTs();
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache2.flush();
		cache.start();
		cache.shiftTs();
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test001_() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar1 = car.addInstance("myCar1");
		ClientCache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		ClientCache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(), AliveConstraintViolationException.class);
		cache2.flush();
	}

	public void test002_() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric myCar = car.addInstance("myCar");
		ClientCache cache = engine.getCurrentCache();
		cache.flush();

		ClientCache cache2 = engine.newCache().start();
		myCar.remove();

		cache.start();
		myCar.remove();
		cache.flush();
		cache2.start();
		try {
			cache2.tryFlush();
		} catch (ConcurrencyControlException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
}
