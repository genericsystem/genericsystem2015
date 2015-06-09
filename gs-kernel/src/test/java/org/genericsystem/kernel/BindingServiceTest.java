package org.genericsystem.kernel;

import java.util.Arrays;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.api.core.exceptions.NotFoundException;
import org.genericsystem.defaults.exceptions.SingularConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class BindingServiceTest extends AbstractTest {

	public void test001() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		assert "Vehicle".equals(vehicle.getValue());
		assert vehicle.isAlive();
	}

	public void test002() {
		Generic engine = new Root();
		engine.addInstance("Vehicle");

		catchAndCheckCause(() -> engine.addInstance("Vehicle"), ExistsException.class);
	}

	public void test003() {
		Generic engine = new Root();
		Generic animal = engine.addInstance("Animal");// Alone type
		Generic machine = engine.addInstance("Machine");
		Generic vehicle = engine.addInstance(machine, "Vehicle");
		Generic robot = engine.addInstance(machine, "Robot");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic bike = engine.addInstance(vehicle, "Bike");
		Generic transformer = engine.addInstance(Arrays.asList(robot, car), "Transformer");
		Generic plasticTransformer = engine.addInstance(transformer, "PlasticTransformer");

		assert !machine.getSubInheritings().contains(animal) : machine.getSubInheritings().info();
		assert machine.getSubInheritings().containsAll(Arrays.asList(machine, vehicle, robot, car, bike, transformer, plasticTransformer)) : machine.getSubInheritings().info();
		assert machine.getSubInheritings().size() == 7 : machine.getSubInheritings().info();

	}

	public void test004() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power");

		Generic myBmw = vehicle.addInstance("myBmw");

		assert myBmw.getAttributes(power).size() == 0 : myBmw.getAttributes(power).info();
	}

	public void test005() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power");
		Generic vehiclePower = vehicle.addAttribute(power, "VehiclePower");

		Generic myBmw = vehicle.addInstance("myBmw");
		Generic v233 = myBmw.addHolder(vehiclePower, 233);

		assert myBmw.getHolders(vehiclePower).contains(v233);
		assert myBmw.getHolders(vehiclePower).size() == 1;

		assert myBmw.getHolders(power).contains(v233);
		assert myBmw.getHolders(power).size() == 1;

	}

	public void test006() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power");
		Generic vehiclePower = vehicle.addAttribute(power, "VehiclePower");

		Generic myBmw = vehicle.addInstance("myBmw");
		Generic v233 = myBmw.addHolder(power, 233);

		assert myBmw.getHolders(vehiclePower).contains(v233);
		assert myBmw.getHolders(vehiclePower).size() == 1;

		assert myBmw.getHolders(power).contains(v233);
		assert myBmw.getHolders(power).size() == 1;

	}

	public void test007() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power");

		vehicle.addAttribute(power, "VehiclePower");
		catchAndCheckCause(() -> power.enableSingularConstraint(ApiStatics.BASE_POSITION), NotFoundException.class);
	}

	public void test008() {
		Generic engine = new Root();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power");
		Generic vehiclePower = vehicle.addAttribute(power, "VehiclePower");

		vehiclePower.enableSingularConstraint(ApiStatics.BASE_POSITION);
		assert vehiclePower.isSingularConstraintEnabled(ApiStatics.BASE_POSITION);
		Generic myBmw = vehicle.addInstance("myBmw");

		Generic v233 = myBmw.addHolder(vehiclePower, 233);
		assert myBmw.getHolders(vehiclePower).contains(v233);
		assert myBmw.getHolders(vehiclePower).size() == 1;
		catchAndCheckCause(() -> myBmw.addHolder(vehiclePower, 234), SingularConstraintViolationException.class);
	}

}
