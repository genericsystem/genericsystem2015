package org.genericsystem.cdi;

import org.genericsystem.mutability.Generic;
import org.testng.annotations.Test;

@Test
public class InjectionTest extends AbstractTest {

	public void test() {
		assert engine != null;
		assert engine.getCurrentCache() != null;
		assert cacheProvider.get() != null;
	}

	public void test2() {
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic carColor = car.setAttribute("outsideColor", color);
		Generic audi = car.addInstance("audi");
		Generic red = color.addInstance("red");
		Generic audiRed = audi.setHolder(carColor, "audiRed", red);
		assert audi.getHolders(carColor).contains(audiRed) : audi.getHolders(carColor);
	}

}
