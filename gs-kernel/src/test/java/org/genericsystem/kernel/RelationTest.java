package org.genericsystem.kernel;

import java.util.List;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RelationTest extends AbstractTest {

	public void test001() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(root.getMetaRelation());
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		myCar.remove();
		assert !myCar.isAlive();
		catchAndCheckCause(() -> carColor.addInstance("myCarColor", myCar, green), AliveConstraintViolationException.class);

	}

	public void test002() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(root.getMetaRelation());
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		assert myCar.isAlive();
		carColor.addInstance("myCarColor", myCar, green);
	}

	public void test003() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("carColor", color);
		assert carColor.equals(car.getRelation("carColor"));
		assert carColor.equals(car.getRelation("carColor"));
		assert carColor.equals(car.getRelation("carColor", color));
	}

	public void test004() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic door = engine.addInstance("Door");
		Generic carColor = car.addRelation("carColor", color);
		Generic carDoor = car.addRelation("carDoor", door);
		assert carColor.equals(car.getRelation("carColor"));
		assert carDoor.equals(car.getRelation("carDoor"));
	}

	public void test005() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("MyAudi");
		Generic largerThan = root.addInstance("LargerThan", car, car);
		assert largerThan.isInstanceOf(root.getMetaRelation());
		Generic myBmwLargerThanMyAudi = largerThan.addInstance("myBmwLargerThanMyAudi", myBmw, myAudi);
		List<Generic> compared = myBmwLargerThanMyAudi.getComponents();
		assert compared.size() == 2 : compared.size();
		assert compared.contains(myAudi) : compared;
		assert compared.contains(myBmw) : compared;
	}

}
