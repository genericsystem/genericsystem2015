package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RemoveOneCacheTest extends AbstractTest {

	public void test001_simpleHolder() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();
		cache.clear();

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test002_simpleHolder() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
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
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		cache.flush();
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		assert myBmw.getHolders(color).size() == 0;

		Generic myBmwGreen = myBmw.addHolder(color, "green");

		catchAndCheckCause(() -> myBmwBlue.remove(), AliveConstraintViolationException.class);

		assert myBmw.getHolders(color).size() == 0;
	}

	public void test003_multipleHolders() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		cache.flush();
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		assert myBmw.getHolders(color).size() == 0;

		Generic myBmwGreen = myBmw.addHolder(color, "green");
		cache.flush();
		catchAndCheckCause(() -> myBmwBlue.remove(), AliveConstraintViolationException.class);
		assert myBmw.getHolders(color).contains(myBmwGreen);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test003_removeAndAdd() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		myBmwRed.remove();
		cache.flush();
		cache.clear();
		Generic myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert !myBmwRed2.equals(myBmwRed);
		assert !myBmwRed2.isAlive();

		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test004_removeAndAddAndRemove() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.flush();
		myBmwRed.remove();
		Generic myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert myBmwRed.isAlive();
		myBmwRed.remove();
		assert !myBmwRed2.isAlive();

		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test005_removeAndAddAndRemove() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		cache.clear();
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		myBmwRed.remove();
		Generic myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert myBmwRed.isAlive();
		assert !myBmwRed2.isAlive();
		myBmwRed.remove();
		assert myBmw.getHolders(color).size() == 0;
	}

	public void test005_removeConcret_withHolder() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		assert color.getInstances().contains(myBmwRed);
		assert color.getInstances().size() == 1;
		cache.flush();
		myBmw.remove();
		cache.clear();
		assert color.getInstances().contains(myBmwRed);
		assert color.getInstances().size() == 1;
	}

	public void test006_removeStructural_withHolder() {
		Engine engine = new Engine();
		Cache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		myBmw.remove();
		cache.clear();
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}
}
