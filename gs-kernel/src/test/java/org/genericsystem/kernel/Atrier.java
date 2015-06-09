package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.DefaultConfig.NoReferentialIntegrityProperty;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class Atrier extends AbstractTest {

	public void test001() {
		Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		final Generic carColor = car.addRelation("CarColor", color);

		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.TARGET_POSITION);
		carColor.enablePropertyConstraint();
		assert carColor.isPropertyConstraintEnabled();

		final Generic myBmw = car.addInstance("myBmw");
		final Generic green = color.addInstance("green");

		myBmw.setLink(carColor, null, color.addInstance("red"));
		myBmw.setLink(carColor, null, color.addInstance("blue"));
		myBmw.setLink(carColor, null, color.addInstance("yellow"));

		Generic myFirstBmwGreen = myBmw.setLink(carColor, "value1", green);
		Generic mySecondBmwGreen = myBmw.setLink(carColor, "value2", green);

		assert !myFirstBmwGreen.isAlive();
		assert mySecondBmwGreen.isAlive();

		assert myBmw.getLinks(carColor).size() == 4 : myBmw.getLinks(carColor);

		assert myBmw.getLink(carColor, "value1") == null : myBmw.getLink(carColor, "value1").getValue();
		assert myBmw.getLink(carColor, "value2") != null : myBmw.getLink(carColor, "value2").getValue();

		carColor.disablePropertyConstraint();
		catchAndCheckCause(() -> myBmw.addLink(carColor, "value3", green), SingularConstraintViolationException.class);
	}

	public void test002() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		final Generic carColor = car.setRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.TARGET_POSITION);

		final Generic myBmw = car.addInstance("myBmw");
		final Generic red = color.addInstance("red");
		Generic white = color.addInstance("white");

		myBmw.setLink(carColor, "myBmwRed", red);
		myBmw.setLink(carColor, "myBmwWhite", white);

		catchAndCheckCause(() -> myBmw.setLink(carColor, "mySecondBmWRed", red), SingularConstraintViolationException.class);

	}

	public void test003() {

		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		Generic time = engine.addInstance("Time");

		final Generic carColorTime = car.setRelation("CarColorDesign", color, time);
		carColorTime.enableSingularConstraint(ApiStatics.TARGET_POSITION);
		assert carColorTime.isSingularConstraintEnabled(ApiStatics.TARGET_POSITION);

		final Generic myBmw = car.addInstance("myBmw");
		final Generic yourAudi = car.addInstance("yourAudi");
		final Generic red = color.addInstance("red");
		final Generic today = time.addInstance("today");
		Generic white = color.addInstance("white");

		myBmw.setLink(carColorTime, "myBmwRedToday", red, today);
		myBmw.setLink(carColorTime, "myBmwWhiteToday", white, today);

		catchAndCheckCause(() -> yourAudi.setLink(carColorTime, "yourAudiRed", red, today), SingularConstraintViolationException.class);
	}

	// public void test004() { //TODO put in cache.
	//
	// final Root engine = new Root();
	// Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	// final Generic carColor = car.setRelation("CarColor", color);
	// carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
	// assert carColor.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
	// carColor.enableReferentialIntegrity(ApiStatics.TARGET_POSITION);
	//
	// final Generic myBmw = car.addInstance("myBmw");
	// final Generic red = color.addInstance("red");
	// red.setLink(carColor, "myFirstBmwRed", myBmw);
	//
	// // catchAndCheckCause(() -> /* TODO monter un cache */red.setLink(carColor, "mySecondBmwRed", myBmw), SingularConstraintViolationException.class);
	// //
	// // // TODO monter un cache
	// // assert red.getLinks(carColor).size() == 1 : red.getLinks(carColor).size();
	// // assert myBmw.getLink(carColor, "myFirstBmwRed") == null : myBmw.getLink(carColor, "myFirstBmwRed").getValue();
	// // assert myBmw.getLink(carColor, "mySecondBmwRed") != null : myBmw.getLink(carColor, "mySecondBmwRed").getValue();
	// // // TODO flusher
	//
	// }

	// public void test005() {
	// final Root engine = new Root();
	// final Generic car = engine.addInstance("Car");
	// Generic color = engine.addInstance("Color");
	//
	// car.addInstance("myBmw");
	// car.addInstance("myAudi");
	// final Generic red = color.addInstance("red");
	//
	// final Generic carColor = car.setRelation("CarColor", color);
	// carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);
	//
	// // TODO ?
	// catchAndCheckCause(() -> {
	// car.setLink(carColor, "CarRed", red);
	// assert red.getLinks(carColor).size() == 2 : red.getLinks(carColor).size();
	// }, SingularConstraintViolationException.class);
	// }

	public void test006() {
		Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		final Generic carColor = car.setRelation("CarColor", color).enableSingularConstraint(ApiStatics.TARGET_POSITION);
		Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic red = color.addInstance("red");
		myBmw.setLink(carColor, "myBmwRed", red);
		assert carColor.isSingularConstraintEnabled(ApiStatics.TARGET_POSITION);

		catchAndCheckCause(() -> myAudi.setLink(carColor, "myAudiRed", red), SingularConstraintViolationException.class);
	}

	public void testMixin5() {
		Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic carPower = car.addAttribute("Power");
		car.addHolder(carPower, 123);

		Generic motorcycle = engine.addInstance("Motorcycle");
		Generic motorcyclePower = motorcycle.addAttribute("Power");

		/*-------------------------------------------------------------------------------------*/

		Generic vehicle = car.updateSupers(engine.addInstance("Vehicle"));
		// TODO : doesn't match anymore
		// Generic vehiclePower = vehicle.addAttribute("Power");

		// assert vehicle.getAttribute("Power").equals(vehiclePower);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		// assert myVehicle.getValues(carPower).equals(123);
		// assert myVehicle.getValues(vehiclePower).equals(123);
		// assert myVehicle.getValues(motorcyclePower).equals(123);
	}

	public void test007() {
		Root engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("Power");
		Generic defaultVehiclePower = vehicle.addHolder(vehiclePower, 123);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic carPower = car.addAttribute(vehiclePower, "Power");

		assert carPower.inheritsFrom(vehiclePower) : carPower.inheritsFrom(vehiclePower);
		assert car.getValues(vehiclePower).first().equals(123) : car.getValues(vehiclePower).info();
		// TODO carPower doesn't inherit from defaultVehiclePower
		// assert car.getHolders(carPower).first().inheritsFrom(defaultVehiclePower) : car.getHolders(carPower);

		// Generic myCar = car.addInstance("myCar");
		// assert myCar.getValues(vehiclePower).first().equals(123);
		// assert myCar.getValues(carPower).first().equals(123) : myCar.getValues(carPower);
		// assert car.getValues(vehiclePower).first().equals(123);
		// assert car.getValues(carPower).first().equals(123) : car.getValues(carPower);

	}
}
