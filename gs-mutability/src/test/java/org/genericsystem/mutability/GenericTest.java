package org.genericsystem.mutability;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;
import org.testng.collections.Lists;

@Test
public class GenericTest extends AbstractTest {

	public void testEngine() {
		Engine engine = new Engine();
		assert engine.isAlive();
		assert engine.getComponents().isEmpty();
		assert engine.getLevel() == 0;
		assert engine.getMeta().equals(engine);
		assert engine.getRoot().equals(engine);
		assert engine.getSupers().isEmpty();
		assert engine.getValue().equals(Statics.ENGINE_VALUE);
		assert engine.isAlive();
		assert engine.isMeta();
		assert engine.isRoot();
	}

	public void testGetInstances() {
		Engine engine = new Engine();
		// assert engine.getInstances().isEmpty();
		Generic vehicleVertex = engine.addInstance("Vehicle");
		Generic powerVehicleVertex = engine.addInstance("Power", vehicleVertex);
		Generic vehicle = engine.getInstances().stream().filter(g -> g.getValue().equals("Vehicle")).findFirst().get();
		// Generic metaAttribut = engine.getInstances().filter(g -> g.getValue().equals("Engine") && g.getCompositesStream().count() == 1).stream().findFirst().get();
		Generic powerVehicle = engine.getMetaAttribute().getInstances().stream().filter(g -> g.getValue().equals("Power")).findFirst().get();
		assert vehicle.isAlive();
		assert vehicleVertex.isAlive();
		assert powerVehicle.isAlive();
		assert powerVehicleVertex.isAlive();
	}

	public void testAddInstance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		assert engine.getInstances().contains(vehicle) : engine.getInstances().stream().collect(Collectors.toList());
		catchAndCheckCause(() -> engine.addInstance("Vehicle"), ExistsException.class);
	}

	public void testSetInstance() {
		Engine engine = new Engine();
		Generic vehicle = engine.setInstance("Vehicle");
		assert engine.getInstances().contains(vehicle);
		assert engine.setInstance("Vehicle").equals(vehicle);
	}

	public void testAddInheriting() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		assert vehicle.getInheritings().contains(car);
	}

	public void testAddAttribute() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		assert vehicle.isAlive();
		Generic car = engine.addInstance(vehicle, "Car");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic carPower = engine.addInstance("CarPower", car);
		assert car.getAttributes(engine).containsAll(Arrays.asList(vehiclePower, carPower)) : car.getAttributes(engine);
		// assert car.getAttributes(engine).size() == 2;
	}

	public void testAddAttributeWithOverride() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic carPower = engine.addInstance(vehiclePower, "CarPower", car);
		assert car.getAttributes(engine).contains(carPower) : carPower.info();
		assert !car.getAttributes(engine).contains(vehiclePower) : carPower.info();
		// assert car.getAttributes(engine).size() == 1;
	}

	public void testAddAttributeWithOverride2() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic sportCar = engine.addInstance(car, "SportCar");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic carPower = engine.addInstance(vehiclePower, "CarPower", car);
		Generic sportCarPower = engine.addInstance(vehiclePower, "SportCarPower", sportCar);
		assert sportCar.getAttributes(engine).containsAll(Arrays.asList(carPower, sportCarPower)) : car.getAttributes(engine) + " " + sportCarPower.info();
		// assert sportCar.getAttributes(engine).size() == 2;
	}

	public void testAddAttributeWithOverride3() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic sportCar = engine.addInstance(car, "SportCar");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic carPower = engine.addInstance(vehiclePower, "CarPower", car);
		Generic sportCarPower = engine.addInstance(carPower, "SportCarPower", sportCar);
		assert sportCar.getAttributes(engine).contains(sportCarPower) : car.getAttributes(engine) + " " + sportCarPower.info();
		assert !sportCar.getAttributes(engine).contains(vehiclePower) : car.getAttributes(engine) + " " + sportCarPower.info();
		assert !sportCar.getAttributes(engine).contains(carPower) : car.getAttributes(engine) + " " + sportCarPower.info();
		// assert sportCar.getAttributes(engine).size() == 1;
	}

	public void testMultiInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic robot = engine.addInstance("Robot");
		Generic transformer = engine.addInstance(Lists.newArrayList(vehicle, robot), "transformer");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic robotPower = engine.addInstance("RobotPower", robot);
		assert transformer.getAttributes(engine).containsAll(Arrays.asList(robotPower, vehiclePower)) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 2;
	}

	public void testMultiInheritance2() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic robot = engine.addInstance("Robot");
		Generic transformer = engine.addInstance(Lists.newArrayList(vehicle, robot), "transformer");
		Generic vehiclePower = engine.addInstance("VehiclePower", vehicle);
		Generic robotPower = engine.addInstance("RobotPower", robot);
		Generic transformerPower = engine.addInstance(Lists.newArrayList(vehiclePower, robotPower), "TransformerPower", transformer);
		assert transformer.getAttributes(engine).contains(transformerPower) : transformer.getAttributes(engine);
		assert !transformer.getAttributes(engine).contains(robotPower) : transformer.getAttributes(engine);
		assert !transformer.getAttributes(engine).contains(vehiclePower) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 1;
	}

	public void testMultiInheritance3() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic robot = engine.addInstance("Robot");
		Generic transformer = engine.addInstance(Lists.newArrayList(vehicle, robot), "transformer");
		engine.addInstance("Power", vehicle);
		engine.addInstance("Power", robot);
		Generic transformerPower = engine.addInstance("Power", transformer);
		assert transformer.getAttributes(engine).contains(transformerPower) : transformer.getAttributes(engine);
		// assert transformer.getAttributes(engine).size() == 1;
	}
}
