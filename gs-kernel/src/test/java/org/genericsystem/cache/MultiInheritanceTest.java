package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class MultiInheritanceTest extends AbstractTest {

	public void test_multiInheritance() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric vehicleSizable = engine.addInstance("Sizable", vehicle);
		ClientGeneric robot = engine.addInstance("Robot");
		ClientGeneric robotSizable = engine.addInstance("Sizable", robot);
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(vehicleSizable);
		assert transformer.getAttributes(engine).contains(robotSizable);
		ClientGeneric transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAttributes(engine).size() == 1 : transformer.getAttributes(engine);
		assert transformer.getAttributes(engine).contains(transformerSizable);
		assert !transformer.getAttributes(engine).contains(robotSizable);
		assert !transformer.getAttributes(engine).contains(vehicleSizable);

	}

	public void test_multiInheritanceWithDiamond() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric objectSizable = engine.addInstance("Sizable", object);
		ClientGeneric vehicle = engine.addInstance(Arrays.asList(object), "Vehicle");
		assert vehicle.inheritsFrom(object);
		ClientGeneric vehicleSizable = engine.addInstance("Sizable", vehicle);
		assert vehicleSizable.inheritsFrom(objectSizable);
		ClientGeneric robot = engine.addInstance(Arrays.asList(object), "Robot");
		assert robot.inheritsFrom(object);
		ClientGeneric robotSizable = engine.addInstance("Sizable", robot);
		assert robotSizable.inheritsFrom(objectSizable);
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert transformer.inheritsFrom(vehicle);
		assert transformer.inheritsFrom(robot);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(vehicleSizable);
		assert transformer.getAttributes(engine).contains(robotSizable);
		ClientGeneric transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(transformerSizable);
		assert !transformer.getAttributes(engine).contains(robotSizable);
		assert !transformer.getAttributes(engine).contains(vehicleSizable);

	}
}
