package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;

import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.testng.annotations.Test;

@Test
public class GetInstanceTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmw = car.addInstance("myBmw");

		assert vehicle.getInstance("myBmw") == null;
		assert car.getInstance("myBmw") == myBmw;
	}

	public void test002() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myBmw = vehicle.addInstance("myBmw");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmwCar = car.addInstance("myBmw");

		assert vehicle.getInstance("myBmw") == myBmw;
		assert car.getInstance("myBmw") == myBmwCar;
	}

	public void test003() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmw115 = vehiclePower.addInstance(115, myBmw);
		vehiclePower.addInstance(116, myBmw);
		vehiclePower.addInstance(116, myAudi);

		assert vehiclePower.getInstance(115) == myBmw115;
	}

	public void test004() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("myBmwRed", myBmw, red);
		vehicleColor.addInstance("myAudiRed", myAudi, red);
		vehicleColor.addInstance("myBmwBlue", myBmw, blue);

		assert vehicleColor.getInstance("myBmwRed") == myBmwRed;
	}

	public void test005() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		catchAndCheckCause(() -> tree.getInstance("children2"), AmbiguousSelectionException.class);
		assert tree.getInstance("children1") == children1;
	}

	public void test006() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		Generic myAudiRed = vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstance("", myBmw, red) == myBmwRed;
		assert vehicleColor.getInstance("", myAudi) == myAudiRed;
		assert vehicleColor.getInstance("", blue) == myBmwBlue;
	}

	public void test007() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		Generic myAudiRed = vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstance(myBmw, red) == myBmwRed;
		assert vehicleColor.getInstance(myAudi) == myAudiRed;
		assert vehicleColor.getInstance(blue) == myBmwBlue;
	}

	public void test008() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic vehicle = root.addInstance("Vehicle");
		Generic carVehicle = root.addInstance(vehicle, "Car");

		assert root.getInstance(Collections.emptyList(), "Car") == car;
		assert root.getInstance(vehicle, "Car") == carVehicle;
		catchAndCheckCause(() -> root.getInstance("Car"), AmbiguousSelectionException.class);
	}

	public void test009() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance("Car");
		Generic carVehicle = root.addInstance(vehicle, "Car");

		Generic carPower = car.addAttribute("power");
		Generic carVehiclePower = carVehicle.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert carVehiclePower.inheritsFrom(vehiclePower);

		catchAndCheckCause(() -> root.getRoot().getMetaAttribute().getInstance("power"), AmbiguousSelectionException.class);
		assert root.getRoot().getMetaAttribute().getInstances("power").size() == 3;
		assert root.getRoot().getMetaAttribute().getInstances("power").containsAll(Arrays.asList(vehiclePower, carPower, carVehiclePower));
		catchAndCheckCause(() -> root.getRoot().getMetaAttribute().getInstance(Collections.emptyList(), "power"), AmbiguousSelectionException.class);
		assert root.getRoot().getMetaAttribute().getInstances(Collections.emptyList(), "power").size() == 2;
		assert root.getRoot().getMetaAttribute().getInstances(Collections.emptyList(), "power").containsAll(Arrays.asList(vehiclePower, carPower));
		assert root.getRoot().getMetaAttribute().getInstance(vehiclePower, "power") == carVehiclePower;
	}

	public void test010() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		assert tree.getInstance(Collections.emptyList(), "children2") == null;
		assert tree.getInstance(Arrays.asList(father), "children2") == children2;
	}

	public void test011() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getInstance("power", trunck) == trunckPower;
		assert root.getRoot().getMetaAttribute().getInstance(Collections.emptyList(), "power", trunck) == trunckPower;
		assert root.getRoot().getMetaAttribute().getInstance(vehiclePower, "power", bike) == bikePower;
	}

	public void test012() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance("Car");
		Generic carVehicle = root.addInstance(vehicle, "Car");

		Generic carPower = car.addAttribute("power");
		Generic carVehiclePower = carVehicle.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert carVehiclePower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getInstance("power", car) == carPower;
		assert root.getRoot().getMetaAttribute().getInstance("power", carVehicle) == carVehiclePower;

		assert root.getRoot().getMetaAttribute().getInstance(Collections.emptyList(), "power", car) == carPower;
		assert root.getRoot().getMetaAttribute().getInstance(vehiclePower, "power", carVehicle) == carVehiclePower;
	}

	public void test013() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehiclePower.addInstance(115, myBmw);
		vehiclePower.addInstance(116, myBmw);
		vehiclePower.addInstance(116, myAudi);

		assert vehiclePower.getInstances(116).size() == 2;
	}

	public void test014() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstances("").size() == 3;
	}

	public void test015() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		assert tree.getInstances("children2").size() == 2;
		assert tree.getInstances("children2").containsAll(Arrays.asList(children2, children3));
	}

	public void test016() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstances("", myBmw).size() == 2;
		assert vehicleColor.getInstances("", myBmw, red).size() == 1;
	}

	public void test017() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstances(myBmw).size() == 2;
		assert vehicleColor.getInstances(blue).size() == 1;
	}

	public void test018() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getInstances(Collections.emptyList(), "power", trunck).first() == trunckPower;
		assert root.getRoot().getMetaAttribute().getInstances(vehiclePower, "power", bike).first() == bikePower;
	}

	public void test019() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance("Car");
		Generic carVehicle = root.addInstance(vehicle, "Car");

		Generic carPower = car.addAttribute("power");
		Generic carVehiclePower = carVehicle.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert carVehiclePower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getInstances(Collections.emptyList(), "power", car).first() == carPower;
		assert root.getRoot().getMetaAttribute().getInstances(vehiclePower, "power", carVehicle).first() == carVehiclePower;
	}

	public void test020() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myBmw = vehicle.addInstance("myBmw");
		vehicle.addInstance("myAudi");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmwCar = car.addInstance("myBmw");
		car.addInstance("myAudi");

		assert vehicle.getSubInstances("myBmw").containsAll(Arrays.asList(myBmw, myBmwCar)) : vehicle.getSubInstances("myBmw").info();
		assert car.getSubInstances("myBmw").first() == myBmwCar;
	}

	public void test021() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		assert tree.getSubInstances("children2").size() == 2;
		assert tree.getSubInstances("children2").containsAll(Arrays.asList(children2, children3));
	}

	public void test022() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getSubInstances("", myBmw).size() == 2;
		assert vehicleColor.getSubInstances("", myBmw).containsAll(Arrays.asList(myBmwRed, myBmwBlue));
	}

	public void test023() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("myBmwBlue", myBmw, blue);

		assert vehicleColor.getSubInstances(myBmw).size() == 2;
		assert vehicleColor.getSubInstances(myBmw).containsAll(Arrays.asList(myBmwRed, myBmwBlue));
	}

	public void test024() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getSubInstances(Collections.emptyList(), "power").size() == 1;
		assert root.getRoot().getMetaAttribute().getSubInstances(Collections.emptyList(), "power").first() == vehiclePower;
		assert root.getRoot().getMetaAttribute().getSubInstances(vehiclePower, "power").size() == 2;
		assert root.getRoot().getMetaAttribute().getSubInstances(vehiclePower, "power").containsAll(Arrays.asList(bikePower, trunckPower));
	}

	public void test025() {
		Root root = new Root();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		assert tree.getSubInstances(Arrays.asList(mother), "children2").size() == 1;
		assert tree.getSubInstances("children2").first() == children2;
	}

	public void test026() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getSubInstances(Collections.emptyList(), "power", vehicle).size() == 1;
		assert root.getRoot().getMetaAttribute().getSubInstances(Collections.emptyList(), "power", vehicle).first() == vehiclePower;
		assert root.getRoot().getMetaAttribute().getSubInstances(vehiclePower, "power", bike).size() == 1;
		assert root.getRoot().getMetaAttribute().getSubInstances(vehiclePower, "power", bike).first() == bikePower;
	}
}
