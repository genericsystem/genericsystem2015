package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class RemoveManyCachesTest extends AbstractTest {

	public void test001_simpleHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		cache.flush();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();

		assert myBmw.getHolders(color).size() == 0;
	}

	public void test002_simpleHolder() {
		ClientEngine engine = new ClientEngine();
		ClientCache cache = engine.getCurrentCache();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		cache.flush();
		ClientCache cache2 = engine.newCache().start();
		ClientGeneric myBmw2 = car.addInstance("myBmw");
		ClientGeneric myBmwRed2 = myBmw2.addHolder(color, "red");
		cache.start();
		cache.shiftTs();
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();

		assert myBmw.getHolders(color).size() == 0;
	}

}
