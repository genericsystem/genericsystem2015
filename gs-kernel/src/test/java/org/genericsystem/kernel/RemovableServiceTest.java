package org.genericsystem.kernel;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RemovableServiceTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		vehicle.remove();

		assert !vehicle.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
	}

	public void test002() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = root.addInstance("MyVehicle");
		myVehicle.remove();

		assert vehicle.isAlive();
		assert !myVehicle.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
		assert root.getCurrentCache().computeDependencies(root).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 1;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
	}

	public void test003() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myFirstVehicle = vehicle.addInstance("myFirstVehicle");
		Generic mySecondVehicle = vehicle.addInstance("MySecondVehicle");
		Generic myThirdVehicle = vehicle.addInstance("MyThirdVehicle");
		mySecondVehicle.remove();
		myFirstVehicle.remove();

		assert vehicle.isAlive();
		assert !myFirstVehicle.isAlive();
		assert !mySecondVehicle.isAlive();
		assert myThirdVehicle.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
		assert root.getCurrentCache().computeDependencies(root).contains(vehicle);
		assert root.getCurrentCache().computeDependencies(root).contains(myThirdVehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 2;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(myThirdVehicle);
		assert myThirdVehicle.getCurrentCache().computeDependencies(myThirdVehicle).stream().count() == 1;
		assert myThirdVehicle.getCurrentCache().computeDependencies(myThirdVehicle).contains(myThirdVehicle);
	}

	public void test004() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		vehicle.addInstance("myVehicle");
		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		car.remove();

		assert vehicle.isAlive();
		assert !car.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
		assert root.getCurrentCache().computeDependencies(root).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 1;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
	}

	public void test006() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		car.remove();

		assert root.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
	}

	public void test007() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		root.addInstance("Power", car);
		car.addInstance("myBmw");
		catchAndCheckCause(() -> car.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test008() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		Generic unit = root.addInstance("Unit", power);

		assert car.isAlive();
		assert power.isAlive();
		assert unit.isAlive();
		car.remove();
		assert !car.isAlive();
		assert !power.isAlive();
		assert !unit.isAlive();
	}

	public void test009() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		root.addInstance(vehicle, "Car");

		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test010() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		vehicleColor.addInstance("CarRed", car, red);

		catchAndCheckCause(() -> vehicleColor.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test011() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		vehicleColor.disableReferentialIntegrity(ApiStatics.TARGET_POSITION);
		red.remove();

		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert !red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test012() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		vehicleColor.addInstance("CarRed", car, red);

		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test013() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);
		carRed.remove();

		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test014() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		vehicle.forceRemove();

		assert !vehicle.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
	}

	public void test015() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("MyVehicle");
		vehicle.forceRemove();

		assert !vehicle.isAlive();
		assert !myVehicle.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
	}

	public void test016() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");

		vehicle.forceRemove();

		assert !vehicle.isAlive();
		assert !car.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
	}

	public void test017() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
		car.forceRemove();

		assert root.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
		assert root.getCurrentCache().computeDependencies(root).contains(root);
		assert !root.getCurrentCache().computeDependencies(root).contains(car);
		assert !root.getCurrentCache().computeDependencies(root).contains(power);
	}

	public void test018() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		vehicleColor.forceRemove();

		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert !vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test019() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);
		car.forceRemove();

		assert root.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test020() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);
		red.forceRemove();

		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert !red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test021() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic animals = root.addInstance("Animals");
		Generic myVehicle = vehicle.addInstance("MyVehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic myCar = car.addInstance("MyCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic green = color.addInstance("Green");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic myCarRed = vehicleColor.addInstance("myCarRed", myCar, red);
		Generic myVehicleGreen = vehicleColor.addInstance("myCarRed", myVehicle, green);

		vehicle.forceRemove();

		assert root.isAlive();
		assert !vehicle.isAlive();
		assert animals.isAlive();
		assert !myVehicle.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
		assert !myCar.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert green.isAlive();
		assert blue.isAlive();
		assert !vehicleColor.isAlive();
		assert !myCarRed.isAlive();
		assert !myVehicleGreen.isAlive();
	}
}
