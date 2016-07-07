package org.genericsystem.remote;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.OptimisticLockConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.Generic;
import org.genericsystem.remote.ClientEngine;
import org.testng.annotations.Test;

@Test
public class NotRemovableManyCachesTest extends AbstractTest {

	public void test001_aliveEx() {
		ClientEngine engine = new ClientEngine();
		AbstractCache cache = engine.getCurrentCache();
		AbstractCache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

	public void test003_aliveEx() {
		ClientEngine engine = new ClientEngine();
		AbstractCache cache = engine.getCurrentCache();
		AbstractCache cache2 = engine.newCache().start();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.start();
		Generic car2 = engine.addInstance("Car2");
		Generic myBmw2 = car2.addInstance("myBmw2");
		catchAndCheckCause(() -> myBmw2.addHolder(color, "red2"), AliveConstraintViolationException.class);
	}

	public void test001_referenceEx() {
		ClientEngine engine = new ClientEngine();
		AbstractCache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		cache.flush();
		AbstractCache cache2 = engine.newCache().start();
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002_referenceEx() {
		ClientEngine engine = new ClientEngine();
		AbstractCache cache = engine.getCurrentCache();
		AbstractCache cache2 = engine.newCache().start();
		AbstractCache cache3 = engine.newCache().start();
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
		ClientEngine engine = new ClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar1 = car.addInstance("myCar1");
		AbstractCache cache1 = engine.getCurrentCache();
		cache1.flush();
		myCar1.remove();
		cache1.flush();
		AbstractCache cache2 = engine.newCache().start();
		catchAndCheckCause(() -> myCar1.remove(), AliveConstraintViolationException.class);
		cache2.flush();
	}

	public void test002_() {
		ClientEngine engine = new ClientEngine();
		Generic car = engine.addInstance("Car");
		Generic myCar = car.addInstance("myCar");
		AbstractCache cache = engine.getCurrentCache();
		cache.flush();
		AbstractCache cache2 = engine.newCache().start();
		myCar.remove();
		cache.start();
		myCar.remove();
		cache.flush();
		cache2.start();
		catchAndCheckCause(() -> cache2.flush(), OptimisticLockConstraintViolationException.class);
		// try {
		// cache2.tryFlush();
		// } catch (ConcurrencyControlException e) {
		//
		// e.printStackTrace();
		// }
	}
}
