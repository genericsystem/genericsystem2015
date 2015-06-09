package org.genericsystem.kernel;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.ExistsException;
import org.testng.annotations.Test;

@Test
public class MultiInheritanceTest extends AbstractTest {

	public void test001() {
		Generic root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehicleSizable = root.addInstance("Sizable", vehicle);
		Generic robot = root.addInstance("Robot");
		Generic robotSizable = root.addInstance("Sizable", robot);
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert transformer.getAttributes(root).contains(vehicleSizable);
		assert transformer.getAttributes(root).contains(robotSizable);
		Generic transformerSizable = root.addInstance("Sizable", transformer);
		assert transformer.getAttributes(root).contains(transformerSizable);
	}

	public void test002() {
		Generic root = new Root();
		Generic object = root.addInstance("Object");
		Generic objectSizable = root.addInstance("Sizable", object);
		Generic vehicle = root.addInstance(Arrays.asList(object), "Vehicle");
		assert vehicle.inheritsFrom(object);
		Generic vehicleSizable = root.addInstance("Sizable", vehicle);
		assert vehicleSizable.inheritsFrom(objectSizable);
		Generic robot = root.addInstance(Arrays.asList(object), "Robot");
		assert robot.inheritsFrom(object);
		Generic robotSizable = root.addInstance("Sizable", robot);
		assert robotSizable.inheritsFrom(objectSizable);
		assert !robotSizable.inheritsFrom(vehicleSizable);
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert transformer.inheritsFrom(vehicle);
		assert transformer.inheritsFrom(robot);
		assert transformer.inheritsFrom(object);
		assert transformer.getAttributes(root).contains(vehicleSizable);
		assert transformer.getAttributes(root).contains(robotSizable);
		Generic transformerSizable = root.addInstance("Sizable", transformer);
		assert transformer.getAttributes(root).contains(transformerSizable);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(car, robot), "Transformer");
		catchAndCheckCause(() -> root.addInstance(Arrays.asList(robot, car), "Transformer"), ExistsException.class);
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic robot = root.addInstance("Robot");
		Generic firstTransformer = root.setInstance(Arrays.asList(car, robot), "Transformer");
		Generic secondTransformer = root.setInstance(Arrays.asList(robot, car), "Transformer");
		assert firstTransformer.equals(secondTransformer);
	}
}
