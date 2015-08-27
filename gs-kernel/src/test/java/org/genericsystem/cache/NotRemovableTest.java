package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class NotRemovableTest extends AbstractClassicTest {

	public void test001_aliveEx() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		catchAndCheckCause(() -> myBmwRed.remove(), AliveConstraintViolationException.class);

	}

	public void test002_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");

		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test003_referenceEx() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}
}
