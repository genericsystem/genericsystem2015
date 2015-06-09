package org.genericsystem.kernel;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class AncestorsServiceTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic device = root.addInstance("Device");
		Generic robot = root.addInstance(device, "Robot");
		Generic transformer = root.addInstance(Arrays.asList(car, robot), "Transformer");
		Generic transformer2 = root.addInstance(transformer, "Transformer2");
		assert transformer.isAncestorOf(transformer2);
		assert robot.isAncestorOf(transformer);
		assert robot.isAncestorOf(transformer2);
		assert device.isAncestorOf(robot);
		assert device.isAncestorOf(transformer);
		assert device.isAncestorOf(transformer2);
		assert car.isAncestorOf(transformer);
		assert car.isAncestorOf(transformer2);
		assert vehicle.isAncestorOf(car);
		assert vehicle.isAncestorOf(transformer);
		assert vehicle.isAncestorOf(transformer2);
		assert root.isAncestorOf(root);
		assert transformer2.isAncestorOf(transformer2);
		assert transformer.isAncestorOf(transformer);
		assert vehicle.isAncestorOf(vehicle);
		assert car.isAncestorOf(car);
		assert robot.isAncestorOf(robot);
		assert device.isAncestorOf(device);
		assert root.isAncestorOf(device);
		assert root.isAncestorOf(robot);
		assert root.isAncestorOf(vehicle);
		assert root.isAncestorOf(car);
		assert root.isAncestorOf(transformer);
		assert root.isAncestorOf(transformer2);
		assert !device.isAncestorOf(car);
		assert !device.isAncestorOf(vehicle);
		assert !device.isAncestorOf(root);
		assert !robot.isAncestorOf(car);
		assert !robot.isAncestorOf(vehicle);
		assert !robot.isAncestorOf(device);
		assert !robot.isAncestorOf(root);
		assert !vehicle.isAncestorOf(robot);
		assert !vehicle.isAncestorOf(device);
		assert !vehicle.isAncestorOf(root);
		assert !car.isAncestorOf(robot);
		assert !car.isAncestorOf(device);
		assert !car.isAncestorOf(vehicle);
		assert !car.isAncestorOf(root);

		assert !transformer.isAncestorOf(robot);
		assert !transformer.isAncestorOf(device);
		assert !transformer.isAncestorOf(car);
		assert !transformer.isAncestorOf(vehicle);
		assert !transformer.isAncestorOf(root);
		assert !transformer2.isAncestorOf(transformer);
		assert !transformer2.isAncestorOf(robot);
		assert !transformer2.isAncestorOf(device);
		assert !transformer2.isAncestorOf(car);
		assert !transformer2.isAncestorOf(vehicle);

		assert device.isAlive();
		assert robot.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert transformer.isAlive();
		assert transformer2.isAlive();
	}

	public void test002() {

		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic microcar = root.addInstance(car, "Microcar");
		assert vehicle.isAncestorOf(car);
		assert vehicle.isAncestorOf(microcar);
		assert car.isAncestorOf(microcar);
		assert vehicle.isAncestorOf(vehicle);
		assert car.isAncestorOf(car);
		assert microcar.isAncestorOf(microcar);
	}

	public void test003() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic airConditioner = root.addInstance("AirConditioner", car);
		Generic microcar = root.addInstance(car, "microcar");
		Generic radio = root.addInstance("Radio", microcar);
		assert vehicle.isAncestorOf(radio);
		assert vehicle.isAncestorOf(airConditioner);
		assert vehicle.isAncestorOf(power);
		assert car.isAncestorOf(car);
		assert car.isAncestorOf(airConditioner);
		assert car.isAncestorOf(radio);
		assert microcar.isAncestorOf(radio);
		assert !car.isAncestorOf(power);
		assert !microcar.isAncestorOf(power);
		assert !microcar.isAncestorOf(airConditioner);
		assert root.isAncestorOf(power);
		assert root.isAncestorOf(airConditioner);
		assert root.isAncestorOf(radio);
	}

	public void test004() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic airConditioner = root.addInstance("AirConditioner", car);
		Generic button = root.addInstance("button", airConditioner);
		Generic microcar = root.addInstance(car, "microcar");
		assert vehicle.isAncestorOf(button);
		assert !microcar.isAncestorOf(button);
		assert root.isAncestorOf(button);
	}
}
