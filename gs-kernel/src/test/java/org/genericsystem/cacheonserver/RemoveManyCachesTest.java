package org.genericsystem.cacheonserver;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonserver.LightClientCache;
import org.genericsystem.distributed.cacheonserver.LightClientEngine;
import org.testng.annotations.Test;

@Test
public class RemoveManyCachesTest extends AbstractTest {

	public void test001_simpleHolder() {
		LightClientEngine engine = new LightClientEngine();
		LightClientCache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		cache.flush();
		LightClientCache cache2 = engine.newCache().start();
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();

		assert myBmw.getHolders(color).size() == 0;
	}

	public void test002_simpleHolder() {
		LightClientEngine engine = new LightClientEngine();
		LightClientCache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		cache.flush();
		LightClientCache cache2 = engine.newCache().start();
		Generic myBmw2 = car.addInstance("myBmw");
		Generic myBmwRed2 = myBmw2.addHolder(color, "red");
		cache.start();
		cache.shiftTs();
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();

		assert myBmw.getHolders(color).size() == 0;
	}

}
