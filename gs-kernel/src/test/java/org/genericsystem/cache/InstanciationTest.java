package org.genericsystem.cache;

import java.util.Arrays;
import java.util.function.Predicate;
import java.util.stream.Stream;

import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class InstanciationTest extends AbstractTest {

	public void testEngineInstanciation() {
		Engine engine = new Engine();
		assert engine.getMeta().equals(engine);
		assert engine.getSupers().isEmpty();
		assert engine.getComponents().isEmpty();
		assert Statics.ENGINE_VALUE.equals(engine.getValue());
		assert engine.isAlive();
		assert engine.isMeta();
	}

	public void testTypeInstanciation() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");

		assert car.getMeta().equals(engine);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(engine);
		assert !car.inheritsFrom(engine);
	}

	public void testTwoTypeInstanciationDifferentNames() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic robot = engine.addInstance("Robot");

		assert car.getMeta().equals(engine);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(engine);
		assert !car.inheritsFrom(engine);

		assert robot.getMeta().equals(engine);
		assert robot.getSupers().isEmpty();
		assert robot.getComposites().isEmpty();
		assert "Robot".equals(robot.getValue());
		assert robot.isAlive();
		assert robot.isStructural();
		assert robot.isInstanceOf(engine);
		assert !robot.inheritsFrom(engine);
	}

	public void testTwoTypeInstanciationSameNamesAddInstance() {
		Engine engine = new Engine();
		engine.addInstance("Car");
		catchAndCheckCause(() -> engine.addInstance("Car"), ExistsException.class);
	}

	public void testTwoTypeInstanciationSameNamesSetInstance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic car2 = engine.setInstance("Car");

		// log.info(engine.info());
		// log.info(car.info());
		// log.info(car2.info());

		assert car.equals(car2);
		assert car.getMeta().equals(engine);
		assert car.getSupers().isEmpty();
		assert car.getComponents().isEmpty();
		assert "Car".equals(car.getValue());
		assert car.isAlive();
		assert car.isStructural();
		assert car.isInstanceOf(engine);
		assert !car.inheritsFrom(engine);

	}

	public void testTwoTypeInstanciationWithInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		// log.info(engine.info());
		// log.info(vehicle.info());
		// log.info(car.info());

		assert vehicle.getMeta().equals(engine);
		assert car.getMeta().equals(engine);

		assert engine.getSupers().isEmpty();
		assert vehicle.getSupers().isEmpty();
		assert car.getSupers().size() == 1;

		assert car.isInstanceOf(engine);
		assert !car.inheritsFrom(engine);

		assert car.inheritsFrom(vehicle);
		assert !car.isInstanceOf(vehicle);
		assert !vehicle.isInstanceOf(car);

		// isAlive test
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();

	}

	public void testTypeInstanciationWithSelfInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		assert !engine.addInstance(Arrays.asList(vehicle), "Vehicle").equals(vehicle);
	}

	public void test3TypeInstanciationWithMultipleInheritence() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic robot = engine.addInstance("Robot");
		Generic transformer = engine.addInstance(Arrays.asList(car, robot), "Transformer");

		// log.info(car.info());
		// log.info(robot.info());
		// log.info(transformer.info());

		assert car.getMeta().equals(engine);
		assert robot.getMeta().equals(engine);
		assert transformer.getMeta().equals(engine);

		assert engine.getSupers().isEmpty();
		assert car.getSupers().isEmpty();
		assert robot.getSupers().isEmpty();
		assert transformer.getSupers().size() == 2;

		assert transformer.getSupers().stream().anyMatch(car::equals); // isAlive test
		assert transformer.getSupers().stream().anyMatch(robot::equals);
		//
		assert car.getComponents().isEmpty();
		assert robot.getComponents().isEmpty();
		assert transformer.getComponents().isEmpty();
		//
		assert engine.isAlive();
		assert car.isAlive();
		assert robot.isAlive();
		assert transformer.isAlive();

	}

	public void test5TypeInstanciationWithMultipleInheritence() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic device = engine.addInstance("Device");
		Generic robot = engine.addInstance(Arrays.asList(device), "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(car, robot), "Transformer");

		// log.info(vehicle.info());
		// log.info(car.info());
		// log.info(device.info());
		// log.info(robot.info());
		// log.info(transformer.info());

		assert car.getMeta().equals(engine);
		assert vehicle.getMeta().equals(engine);
		assert device.getMeta().equals(engine);
		assert robot.getMeta().equals(engine);
		assert transformer.getMeta().equals(engine);

		assert engine.getSupers().isEmpty();
		assert vehicle.getSupers().isEmpty();
		assert car.getSupers().size() == 1;
		assert device.getSupers().isEmpty();
		assert robot.getSupers().stream().count() == 1;
		assert transformer.getSupers().stream().count() == 2;

		assert transformer.getSupers().stream().anyMatch(car::equals);
		assert transformer.getSupers().stream().anyMatch(robot::equals);

		assert car.getSupers().stream().anyMatch(vehicle::equals);
		assert robot.getSupers().stream().anyMatch(device::equals);

		final Predicate<Generic> condition = x -> transformer.getSupers().stream().flatMap(superGeneric -> Stream.concat(Stream.of(superGeneric), superGeneric.getSupers().stream())).anyMatch(x::equals);

		assert condition.test(vehicle);
		assert condition.test(car);
		assert condition.test(robot);
		assert condition.test(device);

		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert device.isAlive();
		assert robot.isAlive();
		assert transformer.isAlive();
	}

	public void test6TypeInstanciationWithMultipleInheritence() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic device = engine.addInstance("Device");
		Generic robot = engine.addInstance(Arrays.asList(device), "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(car, robot), "Transformer");
		Generic transformer2 = engine.addInstance(Arrays.asList(transformer), "Transformer2");

		// log.info(vehicle.info());
		// log.info(car.info());
		// log.info(device.info());
		// log.info(robot.info());
		// log.info(transformer.info());

		assert car.getMeta().equals(engine);
		assert vehicle.getMeta().equals(engine);
		assert device.getMeta().equals(engine);
		assert robot.getMeta().equals(engine);
		assert transformer.getMeta().equals(engine);
		assert transformer2.getMeta().equals(engine);

		assert engine.getSupers().isEmpty();
		assert vehicle.getSupers().isEmpty();
		assert car.getSupers().size() == 1;
		assert device.getSupers().isEmpty();
		assert robot.getSupers().size() == 1;
		assert transformer.getSupers().size() == 2;

		assert transformer.getSupers().stream().anyMatch(car::equals);
		assert transformer.getSupers().stream().anyMatch(robot::equals);

		assert car.getSupers().stream().anyMatch(vehicle::equals);
		assert robot.getSupers().stream().anyMatch(device::equals);

		final Predicate<Generic> condition = x -> transformer.getSupers().stream().flatMap(superGeneric -> Stream.concat(Stream.of(superGeneric), superGeneric.getSupers().stream())).anyMatch(x::equals);

		assert condition.test(vehicle);
		assert condition.test(car);
		assert condition.test(robot);
		assert condition.test(device);

		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert device.isAlive();
		assert robot.isAlive();
		assert transformer.isAlive();
	}
}
