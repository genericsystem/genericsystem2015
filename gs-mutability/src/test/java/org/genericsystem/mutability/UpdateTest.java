package org.genericsystem.mutability;

import java.util.Collections;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UpdateTest extends AbstractTest {

	public void test001_updateValue() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");

		Generic carRename = car.updateValue("CarRename");

		assert carRename.isAlive();
		assert carRename.equals(car);
		assert carRename == car;
		assert car.getValue().equals("CarRename");
		assert car.isAlive();
	}

	public void test002_update() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic otherVehicle = engine.addInstance("OtherVehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		assert car.getSupers().contains(vehicle);
		assert car.getSupers().size() == 1;
		assert vehicle.getInheritings().contains(car);

		Generic carUpdate = car.update(otherVehicle, "Car");

		assert car.getSupers().contains(otherVehicle);
		assert car.getSupers().size() == 1;
		assert otherVehicle.getInheritings().contains(car);

		assert carUpdate.getSupers().contains(otherVehicle);
		assert carUpdate.getSupers().size() == 1;
		assert otherVehicle.getInheritings().contains(carUpdate);

		assert vehicle.getInheritings().size() == 0;
	}

	public void test003_update() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");

		Generic carRename = car.updateValue("CarRename");

		assert carRename.isAlive();
		assert carRename.equals(car);
		assert car.isAlive();
	}

	public void test001_removeAndAdd() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		assert car.isAlive();
		car.remove();
		assert !car.isAlive();
		engine.addInstance("Car");
		assert !car.isAlive();
	}

	public void test001_dependencies() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic bike = engine.addInstance(vehicle, "Bike");
		Generic superCar = engine.addInstance(car, "SuperCar");
		Generic myCar = superCar.addInstance("myCar");
		vehicle.update("newVehicle");
		//
		// assert power.isAlive();
		assert myCar.isAlive();
	}

	public void test001_fusion() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic car2 = engine.addInstance("Car2");
		engine.getCurrentCache().merge(car, Collections.emptyList(), "Car2", Collections.emptyList());
		// car.update("Car2");
		assert car.isAlive();
		assert car2.isAlive();
		car2.update("Car3");
		assert "Car3".equals(car.getValue());
	}

	public void test001_pickNewTs() {
		Engine engine = new Engine();
		Cache cache1 = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		cache1.flush();
		Cache cache2 = engine.newCache().start();
		Generic car2 = engine.getInstances().stream().filter(x -> "Car".equals(x.getValue())).findFirst().get();
		assert car2.isAlive();
		car2.remove();
		assert !car2.isAlive();
		cache2.flush();
		cache1.start();
		assert car.isAlive();
		cache1.shiftTs();
		assert !car.isAlive();
	}

	public void test002_clear() {
		Engine engine = new Engine();
		Cache cache1 = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		cache1.clear();
		assert !car.isAlive();
		catchAndCheckCause(() -> car.addInstance("myCar"), AliveConstraintViolationException.class);
	}
}
