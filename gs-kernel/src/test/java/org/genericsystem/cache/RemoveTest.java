package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class RemoveTest extends AbstractTest {

	public void test001_simpleHolder() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).size() == 1;

		myBmwRed.remove();

		assert myBmw.getHolders(color).size() == 0;
	}

	public void test002_multipleHolders() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");

		myBmwRed.remove();
		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;

		ClientGeneric myBmwGreen = myBmw.addHolder(color, "green");

		myBmwBlue.remove();
		assert myBmw.getHolders(color).contains(myBmwGreen);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test003_removeAndAdd() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");

		myBmwRed.remove();
		myBmwRed = myBmw.addHolder(color, "red");

		assert myBmw.getHolders(color).contains(myBmwRed);
		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 2;
	}

	public void test004_removeAndAddAndRemove() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		ClientGeneric myBmwBlue = myBmw.addHolder(color, "blue");

		myBmwRed.remove();
		assert !myBmwRed.isAlive();
		engine.getCurrentCache().flush();
		assert !myBmwRed.isAlive();
		ClientGeneric myBmwRed2 = myBmw.addHolder(color, "red");
		assert myBmwRed2.isAlive();
		assert myBmwRed.getTs() != myBmwRed2.getTs();
		engine.getCurrentCache().flush();
		assert !myBmwRed.isAlive();
		assert myBmwRed2.isAlive();
		myBmwRed2.remove();

		assert myBmw.getHolders(color).contains(myBmwBlue);
		assert myBmw.getHolders(color).size() == 1;
	}

	public void test005_removeConcret_withHolder() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");

		assert color.getInstances().contains(myBmwRed);
		assert color.getInstances().size() == 1;
		myBmw.remove();

		assert car.getInstances().size() == 0;
		assert color.getInstances().size() == 0;
	}

	public void test006_removeStructural_withHolder() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		myBmw.remove();
		car.remove();

		assert !engine.getInstances().contains(car);
		assert !engine.getInstances().contains(color);
	}

	public void test007_removeConcretAndAttribut() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		myBmw.remove();
		color.remove();

		assert engine.getInstances().contains(car);
		assert !car.getInstances().contains(myBmw);
		assert !engine.getInstances().contains(color);

	}

	public void test008_removeInstanceAndAttribute() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		myBmwRed.remove();
		color.remove();

		assert engine.getInstances().contains(car);
		assert car.getInstances().contains(myBmw);
		assert !engine.getInstances().contains(color);

	}

	public void test009_removeConcret() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = car.addAttribute("Color");
		ClientGeneric myBmw = car.addInstance("myBmw");
		ClientGeneric myBmwRed = myBmw.addHolder(color, "red");
		myBmwRed.remove();

		assert color.getInstances().size() == 0;

	}
}
