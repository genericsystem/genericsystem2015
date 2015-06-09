package org.genericsystem.mutability;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class AncestorsServiceTest extends AbstractTest {

	public void isAncestorOfByInheritence() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic device = engine.addInstance("Device");
		Generic robot = engine.addInstance(device, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(car, robot), "Transformer");
		Generic transformer2 = engine.addInstance(transformer, "Transformer2");

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

		assert engine.isAncestorOf(engine);
		assert transformer2.isAncestorOf(transformer2);
		assert transformer.isAncestorOf(transformer);
		assert vehicle.isAncestorOf(vehicle);
		assert car.isAncestorOf(car);
		assert robot.isAncestorOf(robot);
		assert device.isAncestorOf(device);

		assert engine.isAncestorOf(device);
		assert engine.isAncestorOf(robot);
		assert engine.isAncestorOf(vehicle);
		assert engine.isAncestorOf(car);
		assert engine.isAncestorOf(transformer);
		assert engine.isAncestorOf(transformer2);

		assert !device.isAncestorOf(car);
		assert !device.isAncestorOf(vehicle);
		assert !robot.isAncestorOf(car);
		assert !robot.isAncestorOf(vehicle);
		assert !vehicle.isAncestorOf(robot);
		assert !vehicle.isAncestorOf(device);
		assert !car.isAncestorOf(robot);
		assert !car.isAncestorOf(device);

		assert device.isAlive();
		assert robot.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert transformer.isAlive();
		assert transformer2.isAlive();
	}

	public void isAncestorOfByInheritenceSimpleConfiguration() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");

		assert vehicle.isAncestorOf(car);
		assert vehicle.isAncestorOf(microcar);
		assert car.isAncestorOf(microcar);
		assert microcar.isAncestorOf(microcar);

		assert vehicle.isAncestorOf(vehicle);
		assert car.isAncestorOf(car);
		assert microcar.isAncestorOf(microcar);
	}

	public void isAncestorOfViaComposite() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic airConditioner = engine.addInstance("AirConditioner", car);
		Generic microcar = engine.addInstance(car, "microcar");
		Generic radio = engine.addInstance("Radio", microcar);

		assert vehicle.isAncestorOf(radio);
		assert vehicle.isAncestorOf(airConditioner);
		assert vehicle.isAncestorOf(power);
		assert car.isAncestorOf(car);
		assert car.isAncestorOf(car);
		assert microcar.isAncestorOf(radio);

		assert !car.isAncestorOf(power);
		assert !microcar.isAncestorOf(power);
		assert !microcar.isAncestorOf(airConditioner);

		assert engine.isAncestorOf(power);
		assert engine.isAncestorOf(airConditioner);
		assert engine.isAncestorOf(radio);
	}

	public void isAncestorOfViaComponent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic airConditioner = engine.addInstance("AirConditioner", car);
		Generic button = engine.addInstance("button", airConditioner);

		Generic microcar = engine.addInstance(car, "microcar");
		engine.addInstance("Radio", microcar);

		assert vehicle.isAncestorOf(button);
		assert !microcar.isAncestorOf(button);
		assert engine.isAncestorOf(button);
	}

}
