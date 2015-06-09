package org.genericsystem.kernel;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UpdatableServiceTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehicle2 = vehicle.updateValue("Vehicle2");
		assert "Vehicle2".equals(vehicle2.getValue());
		assert !vehicle.isAlive() : vehicle2.info();
		assert vehicle2.isAlive();
	}

	public void test002() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		String valueCar = "myBmw";
		Generic myBmw = car.addInstance(valueCar);
		String newValue = "myAudi";
		Generic myAudi = car.updateValue(newValue);
		assert !myBmw.isAlive();
		assert myAudi.isAlive();
		assert newValue.equals(myAudi.getValue());
		assert valueCar.equals(myBmw.getValue());
		assert root == myAudi.getMeta();
		assert root.getCurrentCache().computeDependencies(root).contains(myAudi);
		Generic newCar = myAudi.getInstances().iterator().next();
		assert newValue.equals(newCar.getMeta().getValue());
	}

	public void test003() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		car.addInstance("myBmw");
		String robotValue = "Robot";
		Generic robot = root.addInstance(robotValue);
		car.updateValue("myAudi");
		assert robotValue.equals(robot.getValue());
		assert root == robot.getMeta();
		assert robot.getInstances().size() == 0;
		assert robot.isAlive();
	}

	public void test004() {
		Generic root = new Root();
		Generic bike = root.addInstance("Bike");
		Generic car = root.addInstance("Car");
		Generic myBmwBike = bike.addInstance("myBmwBike");
		Generic newBike = bike.updateValue("newBike");

		Collection<Generic> rootAliveDependencies = newBike.getCurrentCache().computeDependencies(newBike);
		assert rootAliveDependencies.size() == 2 : rootAliveDependencies.size();
		assert !rootAliveDependencies.contains(car);
		assert !rootAliveDependencies.contains(myBmwBike);

		Generic getNewBike = root.getInstance("newBike");
		assert getNewBike != null;
		assert root.equals(getNewBike.getMeta());

		Generic getMyBmwBike = getNewBike.getInstance("myBmwBike");
		assert getMyBmwBike != null;
		assert getNewBike.equals(getMyBmwBike.getMeta());
	}

	public void test005() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic myNewBeetle = car.addInstance("myNewBeetle");
		Generic myBmw = car.updateValue("myBmw");

		Collection<Generic> rootAliveDependencies = myBmw.getCurrentCache().computeDependencies(myBmw);
		assert rootAliveDependencies.size() == 2;
		assert !rootAliveDependencies.contains(car);
		assert !rootAliveDependencies.contains(myNewBeetle);

		Generic getMyBmw = root.getInstance("myBmw");
		assert getMyBmw != null;
		assert root.equals(getMyBmw.getMeta());

		Generic getMyNewBeetle = getMyBmw.getInstance("myNewBeetle");
		assert getMyNewBeetle != null;
		assert getMyBmw.equals(getMyNewBeetle.getMeta());
	}

	public void test006() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		Generic options = root.addInstance(car, "Options");

		Generic newCar = car.updateValue("newCar");

		assert newCar.isAlive();
		assert !car.isAlive();
		assert !options.isAlive();

		assert "newCar".equals(newCar.getValue());
		assert root.equals(newCar.getMeta());
		assert root.getCurrentCache().computeDependencies(root).contains(newCar);
		assert newCar.getCurrentCache().computeDependencies(newCar).size() == 2;
		assert newCar.getCurrentCache().computeDependencies(newCar).contains(newCar);
		Generic newOptions = newCar.getCurrentCache().computeDependencies(newCar).stream().collect(Collectors.toList()).get(0);
		assert newOptions.isAlive();
		if ("newCar".equals(newOptions.getValue()))
			newOptions = newCar.getCurrentCache().computeDependencies(newCar).stream().collect(Collectors.toList()).get(1);
		assert root.equals(newOptions.getMeta());
		assert options.getValue().equals(newOptions.getValue());
		List<Generic> newOptionsSupers = newOptions.getSupers();
		assert newOptionsSupers.size() == 1;
		Generic newVehicleFromNewOptions = newOptionsSupers.get(0);
		assert "newCar".equals(newVehicleFromNewOptions.getValue());
	}

	public void test007() {
		Generic root = new Root();
		Generic car = root.addInstance("Car");
		String valuePower = "Power";
		Generic power = root.addInstance(valuePower, car);
		String newValue = "newCar";

		Generic newCar = car.updateValue(newValue);

		assert newValue.equals(newCar.getValue());
		assert !power.isAlive();
		assert root.equals(newCar.getMeta());
		assert root.getCurrentCache().computeDependencies(root).contains(newCar);
		Generic newPower = root.getRoot().getMetaAttribute().getInstance("Power", newCar);
		assert newPower.getComponents().size() == 1;
		Generic compositeOfPower = newPower.getComponents().get(0);
		assert newCar.getValue().equals(compositeOfPower.getValue());
		assert root.equals(compositeOfPower.getMeta());
	}

	public void test008() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		String valuePower = "Power";
		Generic power = root.addInstance(valuePower, car);
		Generic myBmw = car.addInstance("myBmw");
		String newValue = "NewVehicle";

		Generic newVehicle = vehicle.updateValue(newValue);

		assert root.isAlive();
		assert !vehicle.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
		assert !myBmw.isAlive();

		assert root.equals(root.getMeta());
		assert root.equals(vehicle.getMeta());
		assert root.equals(car.getMeta());
		assert root.getRoot().getMetaAttribute().equals(power.getMeta());
		assert car.equals(myBmw.getMeta());

		assert newValue.equals(newVehicle.getValue());
		assert newVehicle.getComponents().size() == 0;
		assert newVehicle.getSupers().isEmpty();
		assert newVehicle.getInstances().size() == 0;
		assert newVehicle.getInheritings().size() == 1;

		Generic newCar = root.getInstance(newVehicle, "Car");
		assert newCar != null;
		assert newCar.getComponents().size() == 0;
		assert newCar.getSupers().size() == 1;
		assert newCar.getInstances().size() == 1;
		assert newCar.getInheritings().size() == 0;

		Generic newPower = root.getRoot().getMetaAttribute().getInstance("Power", newCar);
		assert newPower != null;
		assert newPower.getComponents().size() == 1;
		assert newPower.getSupers().size() == 0;
		assert newPower.getInstances().size() == 0;
		assert newPower.getInheritings().size() == 0;

		Generic myNewBmw = newCar.getInstance("myBmw");
		assert myNewBmw != null;
		assert myNewBmw.getComponents().size() == 0;
		assert myNewBmw.getSupers().size() == 0;
		assert myNewBmw.getInstances().size() == 0;
		assert myNewBmw.getInheritings().size() == 0;
	}

	public void test009() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance("Car");

		assert !car.isSuperOf(car.getMeta(), Collections.singletonList(vehicle), car.getValue(), car.getComponents());
		Generic newCar = car.updateSupers(vehicle);

		assert root.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();
		assert newCar.isAlive();

		Generic newVehicle = root.getInstance("Vehicle");
		assert newVehicle.getInheritings().size() == 1 : newVehicle.getInheritings().info();
		assert root.getInstance(newVehicle, "Car").getSupers().size() == 1;
	}

	public void test010() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic fourWheels = root.addInstance(vehicle, "FourWheels");

		car.updateSupers(fourWheels);

		assert root.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();

		Generic getVehicle = root.getInstance("Vehicle");
		Collection<Generic> newVehicleDependencies = getVehicle.getCurrentCache().computeDependencies(getVehicle);
		assert newVehicleDependencies.size() == 3;
		assert getVehicle.getInheritings().size() == 1;
		assert getVehicle.getSupers().size() == 0;

		Generic getFourWheels = root.getInstance(getVehicle, "FourWheels");
		Collection<Generic> newFourWheelsDependencies = getFourWheels.getCurrentCache().computeDependencies(getFourWheels);
		assert newFourWheelsDependencies.size() == 2;
		assert getFourWheels.getInheritings().size() == 1;
		assert getFourWheels.getSupers().size() == 1;

		Generic getCar = root.getInstance(getFourWheels, "Car");
		assert getCar.getCurrentCache().computeDependencies(getCar).size() == 1;
		assert getCar.getSupers().size() == 1;
		assert getCar.getInheritings().size() == 0;
	}

	public void test011() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");

		car.updateSupers(vehicle);

		assert root.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();

		Generic getVehicle = root.getInstance("Vehicle");
		Collection<Generic> getVehicleDependencies = root.getCurrentCache().computeDependencies(getVehicle);
		assert getVehicleDependencies.size() == 2 : getVehicleDependencies;
		assert getVehicle.getInheritings().size() == 1;

		Generic getCar = root.getInstance(getVehicle, "Car");
		assert getCar.getCurrentCache().computeDependencies(getCar).size() == 1;
		assert getCar.getSupers().size() == 1;
	}

	public void test012() {
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
		Generic myCarRed = vehicleColor.addInstance("MyCarRed", myCar, red);
		Generic myVehicleGreen = vehicleColor.addInstance("MyVehicleGreen", myVehicle, green);

		Generic fourWheels = root.addInstance(vehicle, "FourWheels");
		car.updateSupers(fourWheels);

		assert root.isAlive();
		assert vehicle.isAlive();
		assert animals.isAlive();
		assert myVehicle.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
		assert !myCar.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert green.isAlive();
		assert blue.isAlive();
		assert vehicleColor.isAlive();
		assert !myCarRed.isAlive();
		assert myVehicleGreen.isAlive();
		assert fourWheels.isAlive();

		Generic newVehicle = root.getInstance("Vehicle");
		Collection<Generic> newVehicleDependencies = newVehicle.getCurrentCache().computeDependencies(newVehicle);
		assert newVehicleDependencies.size() == 9;
		assert newVehicle.getInheritings().size() == 1;

		Generic newFourWheels = root.getInstance(newVehicle, "FourWheels");
		assert newFourWheels.getCurrentCache().computeDependencies(newFourWheels).size() == 5;
		assert newFourWheels.getInheritings().size() == 1;
		assert newFourWheels.getSupers().size() == 1;

		Generic newCar = root.getInstance(newFourWheels, "Car");
		assert newCar.getCurrentCache().computeDependencies(newCar).size() == 4;
		assert newCar.getSupers().size() == 1;
	}

	public void test013() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("MyVehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic myCar = car.addInstance("MyCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic green = color.addInstance("Green");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic myCarRed = vehicleColor.addInstance("MyCarRed", myCar, red);
		Generic myVehicleGreen = vehicleColor.addInstance("MyVehicleGreen", myVehicle, green);

		myCarRed.updateComponents(myCar, blue);

		assert root.isAlive();
		assert vehicle.isAlive();
		assert myVehicle.isAlive();
		assert car.isAlive();
		assert power.isAlive();
		assert myCar.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert green.isAlive();
		assert blue.isAlive();
		assert vehicleColor.isAlive();
		assert !myCarRed.isAlive();
		assert myVehicleGreen.isAlive();

		Generic newCarBlue = vehicleColor.getInstance("MyCarRed", myCar, blue);
		List<Generic> newCarBlueComposites = newCarBlue.getComponents();
		assert newCarBlueComposites.size() == 2;
	}

	public void test014() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myCar = car.addInstance("MyCar");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic myCarRed = vehicleColor.addInstance("MyCarRed", myCar, red);
		catchAndCheckCause(() -> myCarRed.updateComponents(blue), MetaRuleConstraintViolationException.class);

	}

	public void test015() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmw = car.addInstance("MyBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic myBmwRed = vehicleColor.addInstance("MyBmwRed", myBmw, red);

		myBmwRed.update("MyBmwBlue", myBmw, blue);

		assert vehicleColor.getInstance("MyBmwRed", myBmw, blue) == null;
		assert vehicleColor.getInstance("MyBmwRed", myBmw, red) == null;
		assert vehicleColor.getInstance("MyBmwBlue", myBmw, red) == null;
		assert vehicleColor.getInstance("MyBmwBlue", myBmw, blue) != null;

		Generic myBmwBlue = vehicleColor.getInstance("MyBmwBlue", myBmw, blue);
		assert myBmwBlue.getCurrentCache().computeDependencies(myBmwBlue).size() == 1;
		List<Generic> newBmwBlueComposites = myBmwBlue.getComponents();
		assert newBmwBlueComposites.size() == 2;
	}

	public void test016() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmw = car.addInstance("MyBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic myBmwRed = vehicleColor.addInstance("MyBmwRed", myBmw, red);
		Generic myBmwBlue = myBmwRed.update("MyBmwBlue", myBmw, blue);
		assert !myBmwRed.isAlive();
		assert myBmwBlue.isAlive();
		assert myBmwBlue.getMeta().equals(vehicleColor);

	}

	public void test017() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic carPower = root.setInstance("Power", car);
		Generic power = root.addInstance("Power");

		assert !carPower.isAlive();
		assert power.isAlive();
		assert !car.getAttributes(root).contains(carPower);
	}
}
