package org.genericsystem.kernel;

import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class ComponentsOrderTest extends AbstractTest {

	public void test001() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		myCar.addLink(carColor, "myCarGreen", green);
	}

	public void test002() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		myCar.setLink(carColor, "myCarGreen", green);
	}

	public void test003() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		green.addLink(carColor, "myCarGreen", myCar);
	}

	public void test004() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		green.setLink(carColor, "myCarColor", myCar);
	}

	public void test005() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		carColor.addInstance("myCarGreen", myCar, green);
	}

	public void test006() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		carColor.setInstance("myCarColor", myCar, green);
	}

	public void test007() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		carColor.addInstance("myCarGreen", green, myCar);
	}

	public void test008() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		carColor.setInstance("myCarColor", green, myCar);
	}

	public void test009() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		final Generic carColor = root.addInstance("CarColor", car, color);
		final Generic green = color.addInstance("green");
		catchAndCheckCause(() -> green.addLink(carColor, "myCarColor", green), MetaRuleConstraintViolationException.class);
	}

	public void test010() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		final Generic largerThan = root.addInstance("largerThan", car, car);
		final Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic myMercedes = car.addInstance("myMercedes");
		final Generic myPorsche = car.addInstance("myPorsche");
		myBmw.setLink(largerThan, "myBmwLargerThanMyAudi", myAudi);
		myMercedes.setLink(largerThan, "myMercedesLargerThanMyBmw", myBmw);
		myBmw.setLink(largerThan, "myBmwLargerThanMymyPorsche", myPorsche);
		List<Generic> smallerThanMyBmw = myBmw.getLinks(largerThan, ApiStatics.BASE_POSITION).stream().map(Generic::getTargetComponent).collect(Collectors.toList());
		assert smallerThanMyBmw.size() == 2;
		assert smallerThanMyBmw.contains(myAudi);
		assert smallerThanMyBmw.contains(myPorsche);
	}

	public void test011() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		final Generic largerThan = root.addInstance("largerThan", car, car);
		final Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic myMercedes = car.addInstance("myMercedes");
		final Generic myPorsche = car.addInstance("myPorsche");
		myBmw.addLink(largerThan, "myBmwLargerThanMyAudi", myAudi);
		myMercedes.addLink(largerThan, "myMercedesLargerThanMyBmw", myBmw);
		myBmw.addLink(largerThan, "myBmwLargerThanMymyPorsche", myPorsche);
		List<Generic> smallerThanMyMbw = myBmw.getLinks(largerThan, ApiStatics.BASE_POSITION).stream().map(Generic::getTargetComponent).collect(Collectors.toList());
		assert smallerThanMyMbw.size() == 2;
		assert smallerThanMyMbw.contains(myAudi);
		assert smallerThanMyMbw.contains(myPorsche);
	}

	public void test012() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		final Generic largerThan = engine.addInstance("largerThan", car, car);
		final Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic myMercedes = car.addInstance("myMercedes");
		final Generic myPorsche = car.addInstance("myPorsche");
		largerThan.setInstance("myBmwLargerThanMyAudi", myBmw, myAudi);
		largerThan.setInstance("myMercedesLargerThanMyBmw", myMercedes, myBmw);
		largerThan.setInstance("myBmwLargerThanMymyPorsche", myBmw, myPorsche);
		List<Generic> smallerThanMyMbw = myBmw.getLinks(largerThan, ApiStatics.BASE_POSITION).stream().map(Generic::getTargetComponent).collect(Collectors.toList());
		assert smallerThanMyMbw.size() == 2;
		assert smallerThanMyMbw.contains(myAudi);
		assert smallerThanMyMbw.contains(myPorsche);
	}

	public void test013() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		final Generic largerThan = engine.addInstance("largerThan", car, car);
		final Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic myMercedes = car.addInstance("myMercedes");
		final Generic myPorsche = car.addInstance("myPorsche");
		largerThan.addInstance("myBmwLargerThanMyAudi", myBmw, myAudi);
		largerThan.addInstance("myMercedesLargerThanMyBmw", myMercedes, myBmw);
		largerThan.addInstance("myBmwLargerThanMymyPorsche", myBmw, myPorsche);
		List<Generic> smallerThanMyMbw = myBmw.getLinks(largerThan, ApiStatics.BASE_POSITION).stream().map(Generic::getTargetComponent).collect(Collectors.toList());
		assert smallerThanMyMbw.size() == 2;
		assert smallerThanMyMbw.contains(myAudi);
		assert smallerThanMyMbw.contains(myPorsche);
	}

}