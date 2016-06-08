package org.genericsystem.spring;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.genericsystem.common.Generic;
import org.junit.Test;

public class InjectionTest extends AbstractTest {
	@Test
	public void test() {

		assertNotNull(engine);
		assertNotNull(engine.getCurrentCache());
		assertNotNull(cacheProvider);
	}

	@Test
	public void test2() {
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic carColor = car.setAttribute("outsideColor", color);
		Generic audi = car.addInstance("audi");
		Generic red = color.addInstance("red");
		Generic audiRed = audi.setHolder(carColor, "audiRed", red);
		// assert audi.getHolders(carColor).contains(audiRed) : audi.getHolders(carColor).info();
		assertTrue(audi.getHolders(carColor).contains(audiRed));
	}

}
