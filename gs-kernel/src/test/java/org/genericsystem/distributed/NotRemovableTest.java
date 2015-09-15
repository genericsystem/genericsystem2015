package org.genericsystem.distributed;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.HeavyClientEngine;
import org.testng.annotations.Test;

@Test
public class NotRemovableTest extends AbstractClassicTest {

	public void test001_aliveEx() {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

	public void test002_referenceEx() {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");

		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test003_referenceEx() {
		HeavyClientEngine engine = new HeavyClientEngine();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}
}
