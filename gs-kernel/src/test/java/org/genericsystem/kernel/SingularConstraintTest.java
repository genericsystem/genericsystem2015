package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class SingularConstraintTest extends AbstractTest {

	public void test000() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addAttribute("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		Generic otherColor = root.addInstance("OtherColor");
		Generic carOtherColor = car.addAttribute("CarOtherColor", otherColor);
		carOtherColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert carColor.isSingularConstraintEnabled(0);
		assert carOtherColor.isSingularConstraintEnabled(0);
	}

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
		assert !carColor.isReferentialIntegrityEnabled(ApiStatics.BASE_POSITION);
		myBmw.addLink(carColor, "carRed", red);
		catchAndCheckCause(() -> myBmw.addLink(carColor, "myBmwYellow", yellow), SingularConstraintViolationException.class);
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
		myBmw.addLink(carColor, "vehicleRed", red);
		myAudi.addLink(carColor, "vehicleRed", red);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
		car.addLink(carColor, "carRed", red);
		catchAndCheckCause(() -> car.addLink(carColor, "carYellow", yellow), SingularConstraintViolationException.class);
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic time = root.addInstance("Time");
		Generic red = color.addInstance("red");
		Generic today = time.addInstance("today");
		Generic yesterday = time.addInstance("yesterday");

		Generic carColor = car.addRelation("CarColor", color, time);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
		myBmw.addLink(carColor, "myBmwRedToday", red, today);
		catchAndCheckCause(() -> myBmw.addLink(carColor, "myBmwRedYesterday", red, yesterday), SingularConstraintViolationException.class);

	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);
		assert carColor.isSingularConstraintEnabled(ApiStatics.TARGET_POSITION);
		myBmw.addLink(carColor, "myBmwRed", red);

		catchAndCheckCause(() -> myAudi.addLink(carColor, "myAudiRed", red), SingularConstraintViolationException.class);
	}

	public void test006() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);

		Generic myBmwRed = myBmw.addLink(carColor, "myBmwRed", red);
		Generic myBmwYellow = myBmw.addLink(carColor, "myBmwYellow", yellow);

		assert myBmw.getLinks(carColor).contains(myBmwRed);
		assert myBmw.getLinks(carColor).contains(myBmwYellow);
		assert myBmw.getLinks(carColor).size() == 2;
	}

	public void test007() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);

		Generic myBmwRed = myBmw.addLink(carColor, "myBmwRed", red);
		Generic myAudiYellow = myAudi.addLink(carColor, "myAudiYellow", yellow);

		assert myBmw.getLinks(carColor).contains(myBmwRed);
		assert myBmw.getLinks(carColor).size() == 1;
		assert myAudi.getLinks(carColor).contains(myAudiYellow);
		assert myAudi.getLinks(carColor).size() == 1;
	}

	public void test008() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);

		myBmw.addLink(carColor, "myBmwRed", red);
		myAudi.addLink(carColor, "myAudiYellow", yellow);

		catchAndCheckCause(() -> myBmw.addLink(carColor, "myBmwYellow", yellow), SingularConstraintViolationException.class);
	}

	public void test009() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");

		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");

		Generic location = root.addInstance("Location");
		Generic outside = location.addInstance("outside");

		Generic carColorLocation = car.addRelation("CarColor", color, location);
		carColorLocation.enableSingularConstraint(ApiStatics.TERNARY_POSITION);

		myBmw.addLink(carColorLocation, "myBmwRedOutside", red, outside);
		catchAndCheckCause(() -> myAudi.addLink(carColorLocation, "myAudiRedOutside", red, outside), SingularConstraintViolationException.class);
	}

	public void test010() {

		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		vehicleColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		myBmw.addLink(vehicleColor, "myBmwRed", red);
		catchAndCheckCause(() -> myBmw.addLink(vehicleColor, "myBmwYellow", yellow), SingularConstraintViolationException.class);

	}

	public void test011() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enablePropertyConstraint();
		Generic carRed = car.addLink(carColor, "CarRed", red);
		assert carRed.isSuperOf(carColor, Collections.emptyList(), "myBmwRed", Arrays.asList(myBmw, red));
		Generic myBmwRed = myBmw.addLink(carColor, "myBmwRed", red);
		assert myBmw.getLinks(carColor).contains(myBmwRed);
		assert myBmw.getLinks(carColor).size() == 1;
		assert red.getLinks(carColor).contains(myBmwRed);
		assert red.getLinks(carColor).size() == 1;
	}

	public void test012() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		Generic carRed = car.addLink(carColor, "CarRed", red);
		assert carRed.isSuperOf(carColor, Collections.emptyList(), "myBmwYellow", Arrays.asList(myBmw, yellow));
		Generic myBmwYellow = myBmw.addLink(carColor, "myBmwYellow", yellow);
		assert myBmw.getLinks(carColor).contains(myBmwYellow);
		assert myBmw.getLinks(carColor).size() == 1;
		assert yellow.getLinks(carColor).contains(myBmwYellow);
		assert yellow.getLinks(carColor).size() == 1;
	}
}
