package org.genericsystem.cache;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class ClassFinderTest extends AbstractClassicTest {

	public void test1() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric vehiclePower = engine.addInstance("VehiclePower", vehicle);
		ClientGeneric carPower = engine.addInstance("CarPower", car);
		assert car.getAttributes(engine).containsAll(Arrays.asList(vehiclePower, carPower)) : car.getAttributes(engine);
		// assert car.getAttributes(engine).size() == 2;
	}

	public void test2() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric vehiclePower = engine.addInstance("Power", vehicle);
		ClientGeneric carPower = engine.addInstance("Power", car);
		assert car.getAttributes(engine).contains(carPower);
		// assert car.getAttributes(engine).size() == 1 : car.getAttributes(engine);
	}

	public void test5() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric vehiclePower = engine.addInstance("VehiclePower", vehicle);
		ClientGeneric carPower = engine.addInstance(vehiclePower, "CarPower", car);
		assert car.getAttributes(engine).contains(carPower);
		// assert car.getAttributes(engine).size() == 1 : car.getAttributes(engine);
	}

	public void test6() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric sportCar = engine.addInstance(car, "SportCar");
		ClientGeneric vehiclePower = engine.addInstance("VehiclePower", vehicle);
		ClientGeneric carPower = engine.addInstance(vehiclePower, "CarPower", car);
		ClientGeneric sportCarPower = engine.addInstance(vehiclePower, "SportCarPower", sportCar);
		assert sportCar.getAttributes(engine).containsAll(Arrays.asList(carPower, sportCarPower)) : car.getAttributes(engine) + " " + sportCarPower.info();
		// assert sportCar.getAttributes(engine).size() == 2;
	}

	public void test7() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric robot = engine.addInstance("robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		ClientGeneric vehiclePower = engine.addInstance("Power", vehicle);
		ClientGeneric robotPower = engine.addInstance("Power", robot);
		assert transformer.getAttributes(engine).containsAll(Arrays.asList(robotPower, vehiclePower)) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 2;
	}

	public void test8() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric robot = engine.addInstance("robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		ClientGeneric vehiclePower = engine.addInstance("VehiclePower", vehicle);
		ClientGeneric robotPower = engine.addInstance("RobotPower", robot);
		ClientGeneric transformerPower = engine.addInstance(Arrays.asList(vehiclePower, robotPower), "TransformerPower", transformer);
		assert transformer.getAttributes(engine).contains(transformerPower) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 1;
	}

	public void test9() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric robot = engine.addInstance("robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		ClientGeneric vehiclePower = engine.addInstance("Power", vehicle);
		ClientGeneric robotPower = engine.addInstance("Power", robot);
		ClientGeneric transformerPower = engine.addInstance("Power", transformer);
		assert transformer.getAttributes(engine).contains(transformerPower) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 1;
		// assert transformer.getAttributes(robot).size() == 0 : transformer.getAttributes(robot);
	}

	public void test10() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		vehicle.remove();

		catchAndCheckCause(() -> engine.addInstance(vehicle, "Car"), AliveConstraintViolationException.class);
	}

	public void test11() {
		ClientGeneric engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		vehicle.remove();

		catchAndCheckCause(() -> vehicle.addInstance("myVehicle"), AliveConstraintViolationException.class);
	}

	// public void test12() {
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// assert car.computeDependencies().contains(car);
	// assert !car.computeDependencies().contains(vehicle);
	// assert !car.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(car);
	// assert vehicle.computeDependencies().contains(vehicle);
	// assert !vehicle.computeDependencies().contains(engine);
	// }

	// public void test13() {
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic sportCar = engine.addInstance(car, "SportCar");
	// assert car.computeDependencies().contains(car);
	// assert !car.computeDependencies().contains(vehicle);
	// assert car.computeDependencies().contains(sportCar);
	// assert !car.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(car);
	// assert vehicle.computeDependencies().contains(vehicle);
	// assert !vehicle.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(sportCar);
	// // assert false : engine.computeAllDependencies();
	// }

	// public void test14() {
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic myCar = car.addInstance("myCar");
	// assert !myCar.isAncestorOf(engine);
	// assert engine.isAncestorOf(myCar);
	// assert car.computeDependencies().contains(car);
	// assert !car.computeDependencies().contains(vehicle);
	// assert car.computeDependencies().contains(myCar);
	// assert !car.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(car);
	// assert vehicle.computeDependencies().contains(vehicle);
	// assert !vehicle.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(myCar);
	// // assert false : engine.computeAllDependencies();
	// }

	// public void test15() {
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic power = engine.addInstance("Power", car);
	// Generic unit = engine.addInstance("Unit", power);
	// assert vehicle.isAncestorOf(unit);
	// assert car.computeDependencies().contains(car);
	// assert !car.computeDependencies().contains(vehicle);
	// assert car.computeDependencies().contains(power);
	// assert car.computeDependencies().contains(unit);
	// assert !car.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(car);
	// assert vehicle.computeDependencies().contains(vehicle);
	// assert !vehicle.computeDependencies().contains(engine);
	// assert vehicle.computeDependencies().contains(power);
	// assert vehicle.computeDependencies().contains(unit);
	// // assert false : engine.computeAllDependencies();
	// }

	// public void test16() {
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic vehiclePower = engine.addInstance("Power", vehicle);
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic myCar = car.addInstance("myCar");
	// Generic v233 = vehiclePower.addInstance(233, myCar);
	// assert v233.isAncestorOf(v233);
	// assert myCar.isAncestorOf(v233);
	// assert car.isAncestorOf(v233);
	// assert vehiclePower.isAncestorOf(v233);
	// assert vehicle.isAncestorOf(v233);
	//
	// Generic car233 = vehiclePower.buildInstance(null, false, Collections.emptyList(), 233, Arrays.asList(car));
	// assert v233.dependsFrom(vehiclePower, 233, Arrays.asList(car));
	// assert !car233.isAlive();
	// assert v233.isAlive();
	// assert !car233.isAncestorOf(v233);
	// assert !car233.computeDependencies().contains(v233);
	// }
}
