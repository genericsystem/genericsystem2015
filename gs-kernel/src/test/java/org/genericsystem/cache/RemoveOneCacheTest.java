package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RemoveOneCacheTest extends AbstractTest {

	public void test001_simpleHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();
		cache.clear();

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test002_simpleHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		cache.clear();
		cache.flush();
		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();
		cache.clear();

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test002_multipleHolders() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		cache.flush();
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		assert myBmw.getHolders(color).size() == 0;

		ClientGeneric myBmwGreen = myBmw.addHolder(color, "green");

		catchAndCheckCause(() -> myBmwBlue.remove(), AliveConstraintViolationException.class);

		assert myBmw.getHolders(color).size() == 0;
	}

	public void test003_multipleHolders() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		cache.flush();
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		assert myBmw.getHolders(color).size() == 0;

		ClientGeneric myBmwGreen = myBmw.addHolder(color, "green");
		cache.flush();
		catchAndCheckCause(() -> myBmwBlue.remove(), AliveConstraintViolationException.class);
		assert myBmw.getHolders(color).contains(myBmwGreen);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test003_removeAndAdd() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");
		myBmwRed.remove();
		cache.flush();
		cache.clear();
		ClientGeneric myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert !myBmwRed2.equals(myBmwRed);
		assert !myBmwRed2.isAlive();

		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test004_removeAndAddAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");
		cache.flush();
		myBmwRed.remove();
		ClientGeneric myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert myBmwRed.isAlive();
		myBmwRed.remove();
		assert !myBmwRed2.isAlive();

		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test005_removeAndAddAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		cache.clear();
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		myBmwRed.remove();
		ClientGeneric myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert myBmwRed.isAlive();
		assert !myBmwRed2.isAlive();
		myBmwRed.remove();
		assert myBmw.getHolders(color).size() == 0;
	}

	public void test005_removeConcret_withHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		assert color.getInstances().contains(myBmwRed);
		assert color.getInstances().size() == 1;
		cache.flush();
		myBmw.remove();
		cache.clear();
		assert color.getInstances().contains(myBmwRed);
		assert color.getInstances().size() == 1;
	}

	public void test006_removeStructural_withHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		myBmw.remove();
		cache.clear();
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}
}
