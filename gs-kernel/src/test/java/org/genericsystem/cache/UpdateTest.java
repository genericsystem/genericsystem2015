package org.genericsystem.cache;

import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UpdateTest extends AbstractTest {

	public void test001_updateValue() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		assert "Car".equals(car.getValue());
		ClientGeneric carRename = car.update("CarRename");
		assert !car.isAlive();
		assert "CarRename".equals(carRename.getValue());
	}

	public void test002_updateValue() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		assert "Car".equals(car.getValue());
		ClientGeneric carRename = car.updateValue("CarRename");
		assert !car.isAlive();
		assert "CarRename".equals(carRename.getValue());
	}

	public void test002_updateMeta() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric myCar = car.addInstance("MyCar");

		ClientGeneric myCarV233 = myCar.addHolder(power, "myCarV233");

		assert myCar.getMeta().equals(car);
		ClientGeneric carUpdate = car.updateValue("CarUpdate");
		assert carUpdate.getInstances().stream().allMatch(x -> "MyCar".equals(x.getValue()));
		assert carUpdate.getInstances().stream().allMatch(x -> x.getHolders(power).stream().allMatch(y -> "myCarV233".equals(y.getValue())));
		assert !myCar.isAlive();
		assert !car.isAlive();
		assert engine.getInstances().contains(carUpdate);
		assert engine.getInstances().size() == 1;
		assert car.getMeta().equals(engine);
	}

	public void test004_updateHolder() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric myCar = car.addInstance("MyCar");
		ClientGeneric v233 = myCar.addHolder(power, 233);

		assert myCar.getComposites().contains(v233);
		assert myCar.getComposites().size() == 1;
		assert v233.getValue().equals(233);

		ClientGeneric v455 = v233.updateValue(455);

		assert !v233.isAlive();
		assert myCar.getComposites().contains(v455);
		assert myCar.getComposites().size() == 1;
		assert v455.getValue().equals(455);
	}

	public void test005_updateSuper() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");

		assert car.getSupers().contains(vehicle);
		ClientGeneric vehicleBis = engine.addInstance("VehicleBis");
		ClientGeneric carBis = car.updateSupers(vehicleBis);

		assert !car.isAlive();
		assert vehicle.isAlive();

		assert car.getSupers().contains(vehicle);
		assert vehicle.getInheritings().size() == 0;

		assert carBis.isAlive();
		assert carBis.getSupers().contains(vehicleBis);

		assert vehicleBis.getInheritings().contains(carBis);
		assert vehicleBis.getInheritings().size() == 1;

	}

	public void test006_attributeToRelation() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric myCar = car.addInstance("MyCar");
		ClientGeneric v233 = myCar.addHolder(power, 233);
		ClientGeneric powerType = engine.addInstance("PowerType");

		engine.getCurrentCache().flush();
		catchAndCheckCause(() -> power.update("carPower", powerType), MetaRuleConstraintViolationException.class);

		assert engine.getInstances().contains(car);
		assert car.isAlive();
		assert car.getInstances().contains(myCar);
		assert myCar.getHolders(power).contains(v233);
		assert myCar.getHolders(power).size() == 1;
		assert v233.getValue().equals(233);

	}

	public void test007_structurel_WithInheritings_AndInstances() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = car.addAttribute("Power");
		ClientGeneric myCar = car.addInstance("myCar");
		ClientGeneric v233 = myCar.addHolder(power, 233);

		ClientGeneric vehicleUpdate = vehicle.update("VehicleUpdate");

		assert vehicleUpdate.isAlive();
		assert !vehicle.isAlive();
		assert engine.getInstances().contains(vehicleUpdate);
		assert engine.getInstances().contains(vehicleUpdate.getInheritings().first());
		assert engine.getInstances().size() == 2;

		assert vehicleUpdate.getInheritings().stream().allMatch(x -> "Car".equals(x.getValue()));
		assert vehicleUpdate.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> "myCar".equals(y.getValue())));
		assert vehicleUpdate.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> y.getHolders(power).stream().allMatch(z -> z.getValue().equals(233))));

		assert v233.getValue().equals(233);

	}
}
