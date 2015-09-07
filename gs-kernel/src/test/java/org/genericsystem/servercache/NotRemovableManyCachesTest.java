package org.genericsystem.servercache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ConcurrencyControlException;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.common.Cache;
import org.genericsystem.kernel.Generic;
import org.genericsystem.kernel.ServerEngine;
import org.testng.annotations.Test;

@Test
public class NotRemovableManyCachesTest extends AbstractTest {

	public void test001_aliveEx() {
		ServerEngine engine = new ServerEngine();
		Cache cache = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

	public void test003_aliveEx() {
		ServerEngine engine = new ServerEngine();
		Cache cache = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		Generic car2 = engine.addInstance("Car2");
		Generic myBmw2 = car2.addInstance("myBmw2");
		catchAndCheckCause(() -> myBmw2.addHolder(color, "red2"), MetaRuleConstraintViolationException.class);
	}

	public void test001_referenceEx() {
		ServerEngine engine = new ServerEngine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		cache.flush();
		Cache cache2 = engine.newCache().start();
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002_referenceEx() {
		ServerEngine engine = new ServerEngine();
		Cache cache = engine.getCurrentCache();
		Cache cache2 = engine.newCache().start();
		Cache cache3 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		cache3.flush();
		cache2.start();
		cache2.shiftTs();
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache2.flush();
		cache.start();
		cache.shiftTs();
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test001_() {
		ServerEngine engine = new ServerEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		Cache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		Cache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(), AliveConstraintViolationException.class);
		cache2.flush();
	}

	public void test002_() {
		ServerEngine engine = new ServerEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		Cache cache = engine.getCurrentCache();
		cache.flush();

		Cache cache2 = engine.newCache().start();
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
