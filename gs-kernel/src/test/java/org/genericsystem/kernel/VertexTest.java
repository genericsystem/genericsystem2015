package org.genericsystem.kernel;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.testng.annotations.Test;

@Test
public class VertexTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");

		assert vehicle.getInheritings().stream().anyMatch(car::equals);
	}

	public void test002() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		List<Generic> list = new ArrayList<>();

		list.add(vehicle);
		list.add(car);
		Generic sportCar = root.addInstance(list, "SportCar");

		assert sportCar.getSupers().contains(car);
		assert sportCar.getSupers().size() == 1;
	}

	public void test003() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		assert root.getInstances().stream().anyMatch(g -> g.equals(vehicle));
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic carPower = root.addInstance("power", car);
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw123 = carPower.addInstance("123", myBmw);

		assert myBmw.getComposites().stream().anyMatch(g -> g.equals(myBmw123));
	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic carPower = root.addInstance("power", car);
		carPower.enablePropertyConstraint();
		Generic myBmw = car.addInstance("myBmw");
		Generic car256 = carPower.addInstance("256", car);
		Generic myBmw123 = carPower.addInstance("123", myBmw);

		assert myBmw.getComposites().contains(myBmw123);
	}

	public void test006() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic otherVehicle = root.addInstance("OtherVehicle");
		assert !otherVehicle.inheritsFrom(vehicle);
		assert vehicle == root.setInstance("Vehicle");
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		assert car.inheritsFrom(vehicle);
		Generic power = root.addInstance("Power", car);
		Generic myBmw = car.addInstance("myBmw");
		assert myBmw.isInstanceOf(car);
		Generic myBmw233 = power.addInstance(233, myBmw);
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic green = color.addInstance("green");
		Generic yellow = color.addInstance("yellow");
		assert !yellow.getSupers().stream().anyMatch(red::equals);
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		assert root.getInstances().containsAll(Arrays.asList(vehicle, car));
		assert car.getInstances().contains(myBmw) : car.getInstances() + car.info();
		assert power.getInstances().contains(myBmw233);
		assert car.getComposites().contains(power);
		assert car.getSupers().stream().findFirst().get() == vehicle : car.getSupers().stream().findFirst().get().info();
		assert car.getSupers().stream().anyMatch(vehicle::equals);
		assert vehicle.getInheritings().contains(car);
		assert myBmw.getComposites().contains(myBmw233);
		assert myBmw.isInstanceOf(car);
		assert myBmw.isInstanceOf(vehicle);
		assert !myBmw.isInstanceOf(root);
		assert vehicle.isInstanceOf(root);
		assert !vehicle.inheritsFrom(root) : vehicle.getLevel() + " " + root.getLevel() + " " + vehicle.equals(root);
		assert car.inheritsFrom(vehicle);
		assert !car.isInstanceOf(vehicle);
		assert !power.inheritsFrom(root);
		assert !myBmw233.inheritsFrom(power);
		assert myBmw233.isInstanceOf(power);
		assert root.getInstance(vehicle, "Car") != null;
		assert power.getInstance(233, myBmw) != null;
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);
		Generic carGreen = vehicleColor.addInstance("CarGreen", car, green);
		assert carRed.isSuperOf(vehicleColor, Arrays.asList(carRed), "myBmwRed", Arrays.asList(myBmw, red));
		assert !carRed.isSuperOf(vehicleColor, Collections.emptyList(), "myBmwRed", Arrays.asList(myBmw, red));
		assert carRed.isSuperOf(vehicleColor, Collections.singletonList(carRed), "CarRed", Arrays.asList(myBmw, red));
		assert carGreen.isInstanceOf(vehicleColor);
		assert vehicleColor.getInstances().contains(carGreen);

		Generic myBmwYellow = vehicleColor.addInstance(carGreen, "CarRed", myBmw, yellow);
		assert carGreen.isSuperOf(vehicleColor, Collections.singletonList(carGreen), "CarRed", Arrays.asList(myBmw, yellow));
		assert myBmwYellow.inheritsFrom(carGreen);

		Generic myBmwRed = vehicleColor.addInstance(carRed, "myBmwRed", myBmw, red);
		assert carRed.isSuperOf(vehicleColor, Collections.singletonList(carRed), "myBmwRed", Arrays.asList(myBmw, red));
		assert vehicleColor.getInstances().contains(myBmwRed);
		assert myBmwRed.inheritsFrom(carRed);
		assert !yellow.inheritsFrom(red);
		assert !yellow.isInstanceOf(red);
		assert myBmwRed == vehicleColor.getInstance("myBmwRed", myBmw, red);
		assert myBmwRed == vehicleColor.setInstance("myBmwRed", myBmw, red);
		assert myBmwRed == vehicleColor.getInstance("myBmwRed", myBmw, red) : vehicleColor.getInstance("myBmwRed", myBmw, red).info();
		assert myBmwRed.inheritsFrom(carRed);
		assert car.getAttributes(root).contains(power) : car.getAttributes(root);
		assert car.getAttributes(root).contains(vehicleColor) : car.getAttributes(root);
		assert !myBmwRed.inheritsFrom(power);
		assert !myBmw233.inheritsFrom(power);
		assert myBmw233.isInstanceOf(power);

		assert myBmw.getHolders(power).contains(myBmw233) : myBmw.getHolders(power);
		assert myBmw.getHolders(power).size() == 1 : myBmw.getHolders(power);
		assert myBmw.getValues(power).contains(233);
	}

	@Test(enabled = false)
	public void test007() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		root.addInstance("OtherVehicle");
		assert vehicle == root.setInstance("Vehicle");
	}

	public void test008() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic vehiclePower = root.addInstance("VehiclePower", vehicle);
		Generic carPower = root.addInstance("CarPower", car);
		assert car.getAttributes(root).containsAll(Arrays.asList(vehiclePower, carPower)) : car.getAttributes(root);
	}

	public void test009() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic vehiclePower = root.addInstance("VehiclePower", vehicle);
		Generic carPower = root.addInstance(vehiclePower, "CarPower", car);
		assert car.getAttributes(root).contains(carPower);
	}

	public void test010() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic sportCar = root.addInstance(car, "SportCar");
		Generic vehiclePower = root.addInstance("VehiclePower", vehicle);
		Generic carPower = root.addInstance(vehiclePower, "CarPower", car);
		Generic sportCarPower = root.addInstance(vehiclePower, "SportCarPower", sportCar);
		assert sportCar.getAttributes(root).containsAll(Arrays.asList(carPower, sportCarPower)) : car.getAttributes(root) + " " + sportCarPower.info();
	}

	public void test011() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic vehiclePower = root.addInstance("Power", vehicle);
		Generic robotPower = root.addInstance("Power", robot);
		assert transformer.getAttributes(root).containsAll(Arrays.asList(robotPower, vehiclePower)) : transformer.getAttributes(root);
	}

	public void test012() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic vehiclePower = root.addInstance("VehiclePower", vehicle);
		Generic robotPower = root.addInstance("RobotPower", robot);
		Generic transformerPower = root.addInstance(Arrays.asList(vehiclePower, robotPower), "TransformerPower", transformer);
		assert transformer.getAttributes(root).contains(transformerPower) : transformer.getAttributes(root);
	}

	public void test013() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.addInstance("Power", vehicle);
		root.addInstance("Power", robot);
		Generic transformerPower = root.addInstance("Power", transformer);
		assert transformer.getAttributes(root).contains(transformerPower) : transformer.getAttributes(root);
	}

	public void test014() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		assert car.getCurrentCache().computeDependencies(car).contains(car);
		assert !car.getCurrentCache().computeDependencies(car).contains(vehicle);
		assert !car.getCurrentCache().computeDependencies(car).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(car);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert !vehicle.getCurrentCache().computeDependencies(vehicle).contains(root);
	}

	public void test015() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic sportCar = root.addInstance(car, "SportCar");
		assert car.getCurrentCache().computeDependencies(car).contains(car);
		assert !car.getCurrentCache().computeDependencies(car).contains(vehicle);
		assert car.getCurrentCache().computeDependencies(car).contains(sportCar);
		assert !car.getCurrentCache().computeDependencies(car).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(car);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert !vehicle.getCurrentCache().computeDependencies(vehicle).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(sportCar);
		assert sportCar.getCurrentCache().computeDependencies(sportCar).contains(sportCar);
		assert !sportCar.getCurrentCache().computeDependencies(sportCar).contains(car);
		assert !sportCar.getCurrentCache().computeDependencies(sportCar).contains(vehicle);
		assert !sportCar.getCurrentCache().computeDependencies(sportCar).contains(root);
	}

	public void test016() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmw = car.addInstance("myBmw");
		assert !myBmw.isAncestorOf(root);
		assert root.isAncestorOf(myBmw);
		assert car.getCurrentCache().computeDependencies(car).contains(car);
		assert !car.getCurrentCache().computeDependencies(car).contains(vehicle);
		assert car.getCurrentCache().computeDependencies(car).contains(myBmw);
		assert !car.getCurrentCache().computeDependencies(car).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(car);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert !vehicle.getCurrentCache().computeDependencies(vehicle).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(myBmw);
	}

	public void test017() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic unit = root.addInstance("Unit", power);
		assert vehicle.isAncestorOf(unit);
		assert car.getCurrentCache().computeDependencies(car).contains(car);
		assert !car.getCurrentCache().computeDependencies(car).contains(vehicle);
		assert car.getCurrentCache().computeDependencies(car).contains(power);
		assert car.getCurrentCache().computeDependencies(car).contains(unit);
		assert !car.getCurrentCache().computeDependencies(car).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(car);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert !vehicle.getCurrentCache().computeDependencies(vehicle).contains(root);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(power);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(unit);
	}

	public void test018() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic myCar = car.addInstance("myCar");
		Generic v233 = vehiclePower.addInstance(233, myCar);
		assert v233.isAncestorOf(v233);
		assert myCar.isAncestorOf(v233);
		assert car.isAncestorOf(v233);
		assert vehiclePower.isAncestorOf(v233);
		assert vehicle.isAncestorOf(v233);
		Generic car233 = car.setHolder(vehiclePower, 233);
		assert car233.isAlive();
		assert !car233.isAncestorOf(v233);
		assert root.getCurrentCache().computePotentialDependencies(vehiclePower, Collections.emptyList(), 233, Arrays.asList(car)).contains(car233);
	}
}
