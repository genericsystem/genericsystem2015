package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class AncestorsServiceTest extends AbstractTest {

	public void isAncestorOfByInheritence() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric device = engine.addInstance("Device");
		ClientGeneric robot = engine.addInstance(device, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(car, robot), "Transformer");
		ClientGeneric transformer2 = engine.addInstance(transformer, "Transformer2");

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
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");

		assert vehicle.isAncestorOf(car);
		assert vehicle.isAncestorOf(microcar);
		assert car.isAncestorOf(microcar);
		assert microcar.isAncestorOf(microcar);

		assert vehicle.isAncestorOf(vehicle);
		assert car.isAncestorOf(car);
		assert microcar.isAncestorOf(microcar);
	}

	public void isAncestorOfViaComposite() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric airConditioner = engine.addInstance("AirConditioner", car);
		ClientGeneric microcar = engine.addInstance(car, "microcar");
		ClientGeneric radio = engine.addInstance("Radio", microcar);

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
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric airConditioner = engine.addInstance("AirConditioner", car);
		ClientGeneric button = engine.addInstance("button", airConditioner);

		ClientGeneric microcar = engine.addInstance(car, "microcar");
		engine.addInstance("Radio", microcar);

		assert vehicle.isAncestorOf(button);
		assert !microcar.isAncestorOf(button);
		assert engine.isAncestorOf(button);
	}

}
