package org.genericsystem.mutability;

import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UpdateWithCacheTest extends AbstractTest {

	public void test004_updateHolder() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myCar = car.addInstance("MyCar");
		Generic v233 = myCar.addHolder(power, 233);

		assert myCar.getComposites().contains(v233);
		assert myCar.getComposites().size() == 1;
		assert v233.getValue().equals(233);

		Generic v455 = v233.updateValue(455);

		assert myCar.getComposites().contains(v455);
		assert myCar.getComposites().size() == 1;
		assert v455.getValue().equals(455);
	}

	public void test005_updateSuper() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");

		assert car.getSupers().contains(vehicle);
		Generic vehicleBis = engine.addInstance("VehicleBis");
		Generic carBis = car.updateSupers(vehicleBis);

		assert car.isAlive();
		assert vehicle.isAlive();

		assert vehicle.getInheritings().size() == 0;

		assert carBis.isAlive();
		assert carBis.getSupers().contains(vehicleBis);

		assert vehicleBis.getInheritings().contains(carBis);
		assert vehicleBis.getInheritings().size() == 1;

	}

	public void test006_attributeToRelation() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic ref = power;
		Generic myCar = car.addInstance("myCar");
		Generic v233 = myCar.addHolder(power, 233);
		Generic ref2 = v233;
		Generic powerType = engine.addInstance("PowerType");
		assert v233.getMeta().equals(power);
		engine.getCurrentCache().flush();
		assert v233.getMeta().equals(power);
		assert v233.getMeta() == power;
		catchAndCheckCause(() -> power.update("carPower", powerType), MetaRuleConstraintViolationException.class);
		assert ref == power;
		assert ref2 == v233;
		assert engine.getInstances().contains(car);
		assert car.isAlive();
		assert car.getInstances().contains(myCar);
		assert power.isAlive();
		assert v233.isAlive();
		assert myCar.isAlive();
		assert myCar.getComposites().contains(v233);
		assert v233.getMeta() == power : v233.getMeta().info();
		assert myCar.getHolders(power).contains(v233) : myCar.getHolders(power).info();
		assert myCar.getHolders(power).size() == 1;
		assert v233.getValue().equals(233);

	}

	public void test007_structurel_WithInheritings_AndInstances() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = car.addAttribute("Power");
		Generic myCar = car.addInstance("myCar");
		Generic v233 = myCar.addHolder(power, 233);

		Generic vehicleUpdate = vehicle.update("VehicleUpdate");

		assert vehicleUpdate.isAlive();
		assert vehicle.isAlive();
		assert engine.getInstances().contains(vehicleUpdate);
		assert engine.getInstances().contains(vehicleUpdate.getInheritings().first());
		assert engine.getInstances().size() == 2;

		assert car.isAlive();
		assert power.isAlive();
		assert myCar.isAlive();

		assert vehicleUpdate.getInheritings().stream().allMatch(x -> "Car".equals(x.getValue()));
		assert vehicleUpdate.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> "myCar".equals(y.getValue())));
		assert vehicleUpdate.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> y.getHolders(power).stream().allMatch(z -> z.getValue().equals(233))));

		engine.getCurrentCache().flush();

		assert v233.getValue().equals(233);

	}

	public void test007_simulateRollback() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		engine.getCurrentCache().flush();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic car = engine.addInstance("Car");
		vehicle.updateValue("VehicleNew");
		vehicle.updateValue("VehicleNew2");
		engine.getCurrentCache().clear();// same as rollback
		assert metaAttribute.isAlive();
		assert !car.isAlive();
		assert vehicle.isAlive();
		assert vehicle.getValue().equals("Vehicle");
	}

}
