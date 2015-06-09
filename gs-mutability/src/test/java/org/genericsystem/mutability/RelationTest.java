package org.genericsystem.mutability;

import java.util.List;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RelationTest extends AbstractTest {

	public void test001_addInstance_NotAliveException() {
		final Engine cache = new Engine();
		Generic car = cache.addInstance("Car");
		Generic color = cache.addInstance("Color");
		final Generic carColor = cache.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		myCar.remove();
		assert !myCar.isAlive();
		catchAndCheckCause(() -> carColor.addInstance("myCarColor", myCar, green), AliveConstraintViolationException.class);
	}

	public void test001_addInstance_NotAliveException_withMetaRelation() {
		final Engine Engine = new Engine();
		Generic metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		Generic car = Engine.addInstance("Car");
		Generic color = Engine.addInstance("Color");
		final Generic carColor = Engine.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(metaRelation);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		myCar.remove();
		assert !myCar.isAlive();
		catchAndCheckCause(() -> carColor.addInstance("myCarColor", myCar, green), AliveConstraintViolationException.class);
	}

	public void test002_addInstance_2composites() {
		final Engine cache = new Engine();
		Generic car = cache.addInstance("Car");
		Generic color = cache.addInstance("Color");
		final Generic carColor = cache.addInstance("CarColor", car, color);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		assert myCar.isAlive();
		carColor.addInstance("myCarColor", myCar, green);
	}

	public void test002_addInstance_2composites_MetaRelation() {
		final Engine Engine = new Engine();
		Generic metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		Generic car = Engine.addInstance("Car");
		Generic color = Engine.addInstance("Color");
		final Generic carColor = Engine.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(metaRelation);
		final Generic myCar = car.addInstance("myCar");
		final Generic green = color.addInstance("green");
		assert myCar.isAlive();
		carColor.addInstance("myCarColor", myCar, green);
	}

	public void test003_addInstance_reflexiveRelation() {
		final Engine Engine = new Engine();
		Generic vehicle = Engine.addInstance("Vehicle");
		Generic car = vehicle.addInstance("Car");
		Generic caravane = vehicle.addInstance("Caravane");
		Generic vehicleHaveSameOwnerAsVehicle = Engine.addInstance("VehicleHaveSameOwnerAsVehicle", vehicle, vehicle);
		Generic myVehicleHaveSameOwnerAsVehicle = vehicleHaveSameOwnerAsVehicle.addInstance("myVehicleHaveSameOwnerAsVehicle", car, caravane);
		List<Generic> composites = myVehicleHaveSameOwnerAsVehicle.getComponents();
		assert composites.size() == 2 : composites.size();
		assert composites.contains(caravane) : composites;
		assert composites.contains(car) : composites;
	}

	public void test003_addInstance_reflexiveRelation_MetaRelation() {
		final Engine Engine = new Engine();
		Generic metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		Generic vehicle = Engine.addInstance("Vehicle");
		Generic car = vehicle.addInstance("Car");
		Generic caravane = vehicle.addInstance("Caravane");
		Generic vehicleHaveSameOwnerAsVehicle = Engine.addInstance("VehicleHaveSameOwnerAsVehicle", vehicle, vehicle);
		assert vehicleHaveSameOwnerAsVehicle.isInstanceOf(metaRelation);
		Generic myVehicleHaveSameOwnerAsVehicle = vehicleHaveSameOwnerAsVehicle.addInstance("myVehicleHaveSameOwnerAsVehicle", car, caravane);
		List<Generic> composites = myVehicleHaveSameOwnerAsVehicle.getComponents();
		assert composites.size() == 2 : composites.size();
		assert composites.contains(caravane) : composites;
		assert composites.contains(car) : composites;
	}

	public void test004_addInheritsRelation() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		assert carColorMat.inheritsFrom(vehicleColor);
		assert vehicleColor.getInheritings().contains(carColorMat);
		assert vehicleColor.getInheritings().size() == 1;
		assert !carColorMat.isInstanceOf(vehicleColor);
	}

	public void test005_addInheritsRelations_OnSameStructural() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		Generic vehicleColorMat = vehicle.addAttribute(vehicleColor, "vehicleColorMat", color);

		assert vehicleColorMat.inheritsFrom(vehicleColor);
		assert vehicleColor.getInheritings().contains(vehicleColorMat);
		assert vehicleColor.getInheritings().size() == 1;
		assert !vehicleColorMat.isInstanceOf(vehicleColor);
	}

	public void test006_inherits_different_than_instance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		assert myVehicleRed.isInstanceOf(vehicleColor);
		assert !myVehicleRed.isInstanceOf(carColorMat);
		assert carColorMat.inheritsFrom(vehicleColor);
	}

	public void test007_addInstance_ofSubRelation() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		Generic myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		assert myCarRedMat.isInstanceOf(carColorMat);
		assert myCarRedMat.isInstanceOf(vehicleColor);
		assert vehicleColor.getInstances().size() == 0;
		assert carColorMat.getInstances().size() == 1;
	}

	public void test008() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		Generic myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		assert carColorMat.getAttributes().contains(vehicleColorIsCold);
	}

	public void test009() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		Generic myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		assert carColorMat.getAttributes().contains(vehicleColorIsCold);
		assert vehicleColor.getAttributes().contains(vehicleColorIsCold);
	}

	public void test010() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic vehiclePower = vehicle.addAttribute("power");
		Generic myVehicle125 = myVehicle.addHolder(vehiclePower, "125");

		assert myVehicle.getHolders(vehiclePower).contains(myVehicle125) : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle125.getValue().equals("125");
		assert myVehicle.getValues(vehiclePower).contains("125") : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle.getHolders(vehiclePower).size() == 1;

		assert myCar.getHolders(vehiclePower).size() == 0;

	}
}
