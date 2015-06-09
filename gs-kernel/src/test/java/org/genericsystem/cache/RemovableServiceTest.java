package org.genericsystem.cache;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RemovableServiceTest extends AbstractTest {

	public void test100_remove_instance_NormalStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicule = engine.addInstance("MyVehicule");

		// when
		myVehicule.remove();

		// then
		assert vehicle.isAlive();
		assert !myVehicule.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 2;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
		assert engine.getCurrentCache().computeDependencies(engine).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 1;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
	}

	public void test101_remove_instance_NormalStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicule1 = vehicle.addInstance("MyVehicule1");
		Generic myVehicule2 = vehicle.addInstance("MyVehicule2");
		Generic myVehicule3 = vehicle.addInstance("MyVehicule3");

		// when
		myVehicule2.remove();
		myVehicule1.remove();

		// then
		assert vehicle.isAlive();
		assert !myVehicule1.isAlive();
		assert !myVehicule2.isAlive();
		assert myVehicule3.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 3;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
		assert engine.getCurrentCache().computeDependencies(engine).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 2;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(myVehicule3);
		assert myVehicule3.getCurrentCache().computeDependencies(myVehicule3).stream().count() == 1;
		assert myVehicule3.getCurrentCache().computeDependencies(myVehicule3).contains(myVehicule3);
	}

	public void test102_remove_typeWithInstance() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.addInstance("MyVehicule");

		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test103_remove_SubType() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");

		// when
		car.remove();

		// then
		assert vehicle.isAlive();
		assert !car.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 2;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
		assert engine.getCurrentCache().computeDependencies(engine).contains(vehicle);
		assert vehicle.getCurrentCache().computeDependencies(vehicle).stream().count() == 1;
		assert vehicle.getCurrentCache().computeDependencies(vehicle).contains(vehicle);
	}

	public void test104_remove_attribute() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");
		assert !engine.getRoot().getMetaAttribute().isReferentialIntegrityEnabled(ApiStatics.BASE_POSITION);

		// when
		vehicle.remove();
		// then
		assert engine.isAlive();
		assert !vehicle.isAlive();
		assert !power.isAlive();
	}

	public void test105_remove_attribute_withInstance_KO() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		engine.addInstance("Power", vehicle);
		vehicle.addInstance("Car");
		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test105_remove_attribute_attribute_KO() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power);

		assert vehicle.isAlive();
		assert power.isAlive();
		assert unit.isAlive();
		vehicle.remove();
		assert !vehicle.isAlive();
		assert !power.isAlive();
		assert !unit.isAlive();
	}

	public void test106_remove_TypeWithSubType_KO() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		engine.addInstance(vehicle, "Car");

		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test107_remove_relation_KO() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		vehicleColor.addInstance("CarRed", car, red);

		catchAndCheckCause(() -> vehicleColor.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test108_remove_relationFromTarget() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);
		engine.getRoot().getMetaRelation().disableReferentialIntegrity(ApiStatics.TARGET_POSITION);

		// when
		red.remove();

		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert !red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test109_remove_link() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		// when
		carRed.remove();

		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test120_remove_Type_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");

		// when
		vehicle.forceRemove();

		// then
		assert !vehicle.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 1;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
	}

	public void test121_remove_typeWithInstance_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("MyVehicule");

		// when
		vehicle.forceRemove();
		// then
		assert !vehicle.isAlive();
		assert !myVehicle.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 1;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
	}

	public void test122_remove_TypeWithSubType_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");

		// when
		vehicle.forceRemove();

		// then
		assert !vehicle.isAlive();
		assert !car.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 1;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
	}

	public void test123_remove_attribute_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);

		// when
		vehicle.forceRemove();

		// then
		assert engine.isAlive();
		assert !vehicle.isAlive();
		assert !power.isAlive();
		// assert engine.computeAllDependencies().stream().count() == 1;
		assert engine.getCurrentCache().computeDependencies(engine).contains(engine);
		assert !engine.getCurrentCache().computeDependencies(engine).contains(vehicle);
		assert !engine.getCurrentCache().computeDependencies(engine).contains(power);
	}

	public void test124_remove_relation_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		// when
		vehicleColor.forceRemove();

		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert !vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test125_remove_instanceBaseOfRelation_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		// when
		car.forceRemove();

		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test126_remove_instanceBaseOfRelation_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic carRed = vehicleColor.addInstance("CarRed", car, red);

		// when
		red.forceRemove();

		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert car.isAlive();
		assert color.isAlive();
		assert !red.isAlive();
		assert vehicleColor.isAlive();
		assert !carRed.isAlive();
	}

	public void test127_remove_typeWithlinks_ForceStrategy() {
		// given
		Generic engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic animals = engine.addInstance("Animals");
		Generic myVehicle = vehicle.addInstance("MyVehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic myCar = car.addInstance("MyCar");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("Red");
		Generic green = color.addInstance("Green");
		Generic blue = color.addInstance("Blue");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic myCarRed = vehicleColor.addInstance("myCarRed", myCar, red);
		Generic myVehicleGreen = vehicleColor.addInstance("myCarRed", myVehicle, green);

		// when
		vehicle.forceRemove();

		// then
		assert engine.isAlive();
		assert !vehicle.isAlive();
		assert animals.isAlive();
		assert !myVehicle.isAlive();
		assert !car.isAlive();
		assert !power.isAlive();
		assert !myCar.isAlive();
		assert color.isAlive();
		assert red.isAlive();
		assert green.isAlive();
		assert blue.isAlive();
		assert !vehicleColor.isAlive();
		assert !myCarRed.isAlive();
		assert !myVehicleGreen.isAlive();
	}

	// public void test130_remove_Type_ConserveStrategy() {
	// // given
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	//
	// // when
	// vehicle.remove(RemoveStrategy.CONSERVE);
	//
	// // then
	// assert !vehicle.isAlive();
	// // assert engine.computeAllDependencies().stream().count() == 1;
	// assert engine.computeAllDependencies().contains(engine);
	// }
	//
	// public void test131_remove_SubType_ConserveStrategy() {
	// // given
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = vehicle.addInstance("Car");
	//
	// // when
	// vehicle.remove(RemoveStrategy.CONSERVE);
	//
	// // then
	// assert !vehicle.isAlive();
	// assert !car.isAlive();
	//
	// List<Generic> engineDependencies = engine.computeAllDependencies().stream().collect(Collectors.toList());
	// // assert engineDependencies.size() == 2;
	// Generic newCar = engine.getInstance("Car");
	// assert newCar.isAlive();
	// assert "Car".equals(newCar.getValue());
	// assert newCar.computeAllDependencies().size() == 1;
	// assert newCar.computeAllDependencies().contains(newCar);
	// }
	//
	// public void test132_remove_with2SubTypes_ConserveStrategy() {
	// // given
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic automatic = engine.addInstance(vehicle, "Automatic");
	//
	// // when
	// vehicle.remove(RemoveStrategy.CONSERVE);
	//
	// // then
	// assert !vehicle.isAlive();
	// assert car.isAlive();
	// assert automatic.isAlive();
	//
	// List<Generic> engineDependencies = engine.computeAllDependencies().stream().collect(Collectors.toList());
	// // assert engineDependencies.size() == 3;
	// // assert engine.getAllInstances().count() == 2 : engine.getAllInstances();
	//
	// Generic newCar = engine.getInstance("Car");
	// assert newCar.isAlive();
	// assert "Car".equals(newCar.getValue());
	// assert newCar.computeAllDependencies().size() == 1;
	// assert newCar.getAllInstances().count() == 0;
	// assert newCar.computeAllDependencies().contains(newCar);
	//
	// Generic newAutomatic = engine.getInstance("Automatic");
	// assert newAutomatic.isAlive();
	// assert "Automatic".equals(newAutomatic.getValue());
	// assert newAutomatic.computeAllDependencies().size() == 1;
	// assert newAutomatic.getAllInstances().count() == 0;
	// assert newAutomatic.computeAllDependencies().contains(newAutomatic);
	// }
	//
	// public void test133_remove_SubSubTypes_ConserveStrategy() {
	// // given
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	// Generic automatic = engine.addInstance(car, "Automatic");
	//
	// // when
	// vehicle.remove(RemoveStrategy.CONSERVE);
	//
	// // then
	// assert !vehicle.isAlive();
	// assert car.isAlive();
	// assert automatic.isAlive();
	//
	// List<Generic> engineDependencies = engine.computeAllDependencies().stream().collect(Collectors.toList());
	// // assert engineDependencies.size() == 3;
	// // assert engine.getAllInstances().count() == 2;
	//
	// Generic newCar = engine.getInstance("Car");
	// assert newCar.isAlive();
	// assert "Car".equals(newCar.getValue());
	// assert newCar.computeAllDependencies().size() == 2;
	// assert newCar.getSupersStream().count() == 0;
	//
	// Generic newAutomatic = engine.getInstance("Automatic");
	// assert newAutomatic.isAlive();
	// assert "Automatic".equals(newAutomatic.getValue());
	// assert newAutomatic.computeAllDependencies().size() == 1;
	// assert newAutomatic.getSupersStream().count() == 1;
	// assert newAutomatic.getSupers().contains(newCar);
	// }
	//
	// public void test134_remove_TypeWithAttribute_ConserveStrategy() {
	// // given
	// Generic engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = vehicle.addInstance("Car");
	// Generic options = engine.addInstance(vehicle, "Options");
	//
	// // when
	// vehicle.remove(RemoveStrategy.CONSERVE);
	//
	// // then
	// assert !vehicle.isAlive();
	// assert options.isAlive();
	// assert !car.isAlive();
	//
	// List<Generic> engineDependencies = engine.computeAllDependencies().stream().collect(Collectors.toList());
	// // assert engineDependencies.size() == 3;
	// // assert engine.getAllInstances().count() == 2;
	//
	// Generic newCar = engine.getInstance("Car");
	// Generic newOptions = engine.getInstance("Options");
	// assert newCar != null;
	// assert newCar.getInheritings().stream().count() == 1;
	// assert newOptions.equals(newCar.getInheritings().stream().collect(Collectors.toList()).get(0));
	//
	// assert newOptions != null;
	// assert newOptions.getSupersStream().count() == 1;
	// assert newCar.equals(newOptions.getSupers().get(0));
	//
	// }

	public void testRemove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.remove();
		catchAndCheckCause(() -> engine.addInstance(vehicle, "Car"), AliveConstraintViolationException.class);
	}
}
