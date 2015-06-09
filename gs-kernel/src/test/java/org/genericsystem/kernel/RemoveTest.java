package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RemoveTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		Generic carColor = car.addRelation("CarColor", color);
		Generic carRed = car.addLink(carColor, "carRed", red);
		myBmw.addLink(carColor, carRed, "myBmwRed", red);

		carRed.conserveRemove();
		Generic myBmwRed = myBmw.getLink(carColor, "myBmwRed", red);
		assert myBmwRed != null;
		assert myBmwRed.isAlive();
		assert myBmwRed.getSupers().size() == 0;
		assert !carRed.isAlive();
		assert carColor.isAlive();
	}

	public void test002() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic outsideColor = root.addInstance(color, "OutsideColor");

		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		Generic outsideRed = outsideColor.addInstance("OutsideRed");

		Generic carColor = car.addRelation("carColor", color);
		Generic carRed = car.addLink(carColor, "carRed", red);

		Generic carOutsideColor = car.addRelation(carColor, "CarOutsideColor", outsideColor);
		Generic carOutsideRed = car.addLink(carOutsideColor, "carOutsideRed", outsideRed);

		myBmw.addLink(carOutsideColor, carOutsideRed, "myBmwOutsideRed", outsideRed);

		carRed.conserveRemove();
		Generic myBmwRed = myBmw.getLink(carOutsideColor, "myBmwOutsideRed", outsideRed);
		assert myBmwRed != null;
		assert myBmwRed.isAlive();
		assert myBmwRed.getSupers().size() == 1;
		assert myBmwRed.getSupers().get(0).equals(carOutsideColor, Collections.emptyList(), "carOutsideRed", Arrays.asList(car, outsideRed)) : myBmwRed.getSupers().get(0).info();
		assert !carRed.isAlive();
	}

	public void test003() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.getMeta().disableReferentialIntegrity(ApiStatics.TARGET_POSITION);
		color.conserveRemove();

		Generic newCarColor = car.getAttribute("CarColor");
		assert !carColor.isAlive();
		assert newCarColor.isAlive();
		assert newCarColor.getComponents().size() == 1 : newCarColor.info();
		assert newCarColor.getComponent(ApiStatics.BASE_POSITION).equals(car) : car.info() + " " + car.getComposites();
	}

	/**
	 * TODO tests à déplacer dans la seconde classe remove
	 */

	public void test004() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		myBmw.addHolder(power, 123);

		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");

		assert myBmw.getHolders(power).contains(myBmw123) : myBmw.getHolders(power).info();
		assert myBmw.getHolders(power).size() == 1;

		myBmw123.remove();

		assert myBmw.getHolders(power).size() == 0;
	}

	public void test006() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");
		Generic myBmw233 = myBmw.addHolder(power, "233");

		myBmw123.remove();
		assert !myBmw.getHolders(power).contains(myBmw123);
		assert myBmw.getHolders(power).contains(myBmw233);
		assert myBmw.getHolders(power).size() == 1;

		Generic myBmw126 = myBmw.addHolder(power, "126");

		myBmw233.remove();
		assert myBmw.getHolders(power).contains(myBmw126);
		assert myBmw.getHolders(power).size() == 1;
	}

	public void test007() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");
		Generic myBmw233 = myBmw.addHolder(power, "233");

		myBmw123.remove();
		myBmw123 = myBmw.addHolder(power, "123");

		assert myBmw.getHolders(power).contains(myBmw123);
		assert myBmw.getHolders(power).contains(myBmw233);
		assert myBmw.getHolders(power).size() == 2;
	}

	public void test008() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");
		Generic myBmw233 = myBmw.addHolder(power, "233");

		myBmw123.remove();
		myBmw123 = myBmw.addHolder(power, "123");
		myBmw123.remove();

		assert !myBmw.getHolders(power).contains(myBmw123);
		assert myBmw.getHolders(power).contains(myBmw233);
		assert myBmw.getHolders(power).size() == 1;
	}

	public void test009() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");

		assert power.getInstances().contains(myBmw123);
		assert power.getInstances().size() == 1;
		myBmw.remove();

		assert car.getInstances().size() == 0;
		assert power.getInstances().size() == 0;
	}

	public void test010() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		myBmw.addHolder(power, "123");
		myBmw.remove();
		car.remove();

		assert !root.getInstances().contains(car);
		assert !root.getInstances().contains(power);
		assert !car.getInstances().contains(myBmw);
	}

	public void test011() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		myBmw.addHolder(power, "123");
		myBmw.remove();
		power.remove();

		assert root.getInstances().contains(car);
		assert !car.getInstances().contains(myBmw);
		assert !root.getInstances().contains(power);

	}

	public void test012() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");
		myBmw123.remove();
		power.remove();

		assert root.getInstances().contains(car);
		assert car.getInstances().contains(myBmw);
		assert !root.getInstances().contains(power);

	}

	public void test013() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = myBmw.addHolder(power, "123");
		myBmw123.remove();

		assert power.getInstances().size() == 0;

	}

	public void test014() {
		Root root = new Root();

		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.addRelation("CarColor", color);

		// Disable default referential integrity for vehicle in vehicleColor for the first target : color
		root.getMetaRelation().disableReferentialIntegrity(ApiStatics.TARGET_POSITION);

		// Enable cascade remove for Color in vehicleColor
		root.getMetaRelation().enableCascadeRemove(ApiStatics.TARGET_POSITION);

		// Remove the type vehicle
		car.remove();
		assert !car.isAlive();
		assert !carColor.isAlive();
		assert !color.isAlive();
	}
}
