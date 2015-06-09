package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.genericsystem.api.core.exceptions.ExistsException;
import org.testng.annotations.Test;

@Test
public class InstanciationTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		assert root.getMeta().equals(root);
		assert root.getComponents().isEmpty();
		assert Statics.ENGINE_VALUE.equals(root.getValue());
		assert root.isAlive();
		assert root.isMeta();
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		assert root.getInstance("Car") == car;
		assert car.getMeta().equals(root);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(root);
		assert !car.inheritsFrom(root);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.setInstance("Car");
		assert root.getInstance("Car") == car;
		assert car.getMeta().equals(root);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(root);
		assert !car.inheritsFrom(root);
	}

	public void test004() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic robot = root.addInstance("Robot");
		assert car.getMeta().equals(root);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(root);
		assert !car.inheritsFrom(root);
		assert robot.getMeta().equals(root);
		assert robot.getSupers().isEmpty();
		assert robot.getComponents().isEmpty();
		assert "Robot".equals(robot.getValue());
		assert robot.isAlive();
		assert robot.isStructural();
		assert robot.isInstanceOf(root);
		assert !robot.inheritsFrom(root);
	}

	public void test005() {
		Root root = new Root();
		root.addInstance("Car");
		catchAndCheckCause(() -> root.addInstance("Car"), ExistsException.class);
	}

	public void test006() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic otherCar = root.setInstance("Car");
		assert car == otherCar;
		assert car.getMeta().equals(root);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(root);
		assert !car.inheritsFrom(root);
	}

	public void test007() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		assert vehicle.getMeta().equals(root);
		assert car.getMeta().equals(root);
		assert root.getSupers().isEmpty();
		assert vehicle.getSupers().isEmpty();
		assert car.getSupers().size() == 1;
		assert car.isInstanceOf(root);
		assert !car.inheritsFrom(root);
		assert car.inheritsFrom(vehicle);
		assert !car.isInstanceOf(vehicle);
		assert !vehicle.isInstanceOf(car);
		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
	}

	public void test008() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic otherCar = root.addInstance(car, "Car");
		assert car.isAlive();
		assert root.getInstance(Collections.emptyList(), "Car").equals(car);
		assert root.getInstance(car, "Car").equals(otherCar);
	}

	public void test009() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic robot = root.addInstance("Robot");
		Generic transformer = root.addInstance(Arrays.asList(car, robot), "Transformer");
		assert car.getMeta().equals(root);
		assert robot.getMeta().equals(root);
		assert transformer.getMeta().equals(root);
		assert root.getSupers().isEmpty();
		assert car.getSupers().isEmpty();
		assert robot.getSupers().isEmpty();
		assert transformer.getSupers().size() == 2;
		assert transformer.getSupers().stream().anyMatch(car::equals); // isAlive test
		assert transformer.getSupers().stream().anyMatch(robot::equals);
		assert car.getComponents().isEmpty();
		assert robot.getComponents().isEmpty();
		assert transformer.getComponents().isEmpty();
		assert root.isAlive();
		assert car.isAlive();
		assert robot.isAlive();
		assert transformer.isAlive();
	}

	public void test010() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic device = root.addInstance("Device");
		Generic robot = root.addInstance(Arrays.asList(device), "Robot");
		Generic transformer = root.addInstance(Arrays.asList(car, robot), "Transformer");
		Generic minitransformer = root.addInstance(Arrays.asList(transformer), "Minitransformer");
		assert car.getMeta().equals(root);
		assert vehicle.getMeta().equals(root);
		assert device.getMeta().equals(root);
		assert robot.getMeta().equals(root);
		assert transformer.getMeta().equals(root);
		assert minitransformer.getMeta().equals(root);
		assert root.getSupers().isEmpty();
		assert vehicle.getSupers().isEmpty();
		assert car.getSupers().size() == 1;
		assert device.getSupers().isEmpty();
		assert robot.getSupers().size() == 1;
		assert transformer.getSupers().size() == 2;
		assert transformer.getSupers().stream().anyMatch(car::equals);
		assert transformer.getSupers().stream().anyMatch(robot::equals);
		assert car.getSupers().stream().anyMatch(vehicle::equals);
		assert robot.getSupers().stream().anyMatch(device::equals);
		Predicate<Generic> condition = x -> transformer.getSupers().stream().flatMap(superVertex -> Stream.concat(Stream.of(superVertex), superVertex.getSupers().stream())).anyMatch(x::equals);
		assert condition.test(vehicle);
		assert condition.test(car);
		assert condition.test(robot);
		assert condition.test(device);
		assert root.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert device.isAlive();
		assert robot.isAlive();
		assert transformer.isAlive();
	}
}
