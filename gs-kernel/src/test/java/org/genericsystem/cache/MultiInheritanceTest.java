package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class MultiInheritanceTest extends AbstractTest {

	public void test_multiInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
		Generic robot = engine.addInstance("Robot");
		Generic robotSizable = engine.addInstance("Sizable", robot);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(vehicleSizable);
		assert transformer.getAttributes(engine).contains(robotSizable);
		Generic transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAttributes(engine).size() == 1 : transformer.getAttributes(engine);
		assert transformer.getAttributes(engine).contains(transformerSizable);
		assert !transformer.getAttributes(engine).contains(robotSizable);
		assert !transformer.getAttributes(engine).contains(vehicleSizable);

	}

	public void test_multiInheritanceWithDiamond() {
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic objectSizable = engine.addInstance("Sizable", object);
		Generic vehicle = engine.addInstance(Arrays.asList(object), "Vehicle");
		assert vehicle.inheritsFrom(object);
		Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
		assert vehicleSizable.inheritsFrom(objectSizable);
		Generic robot = engine.addInstance(Arrays.asList(object), "Robot");
		assert robot.inheritsFrom(object);
		Generic robotSizable = engine.addInstance("Sizable", robot);
		assert robotSizable.inheritsFrom(objectSizable);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert transformer.inheritsFrom(vehicle);
		assert transformer.inheritsFrom(robot);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(vehicleSizable);
		assert transformer.getAttributes(engine).contains(robotSizable);
		Generic transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(transformerSizable);
		assert !transformer.getAttributes(engine).contains(robotSizable);
		assert !transformer.getAttributes(engine).contains(vehicleSizable);

	}
}
