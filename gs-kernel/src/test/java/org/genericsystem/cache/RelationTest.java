package org.genericsystem.cache;

import java.util.List;

import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RelationTest extends AbstractTest {

	public void test001_addInstance_NotAliveException() {
		final ClientEngine cache = new ClientEngine();
		ClientGeneric car = cache.addInstance("Car");
		ClientGeneric color = cache.addInstance("Color");
		final ClientGeneric carColor = cache.addInstance("CarColor", car, color);
		final ClientGeneric myCar = car.addInstance("myCar");
		final ClientGeneric green = color.addInstance("green");
		myCar.remove();
		assert !myCar.isAlive();
		catchAndCheckCause(() -> carColor.addInstance("myCarColor", myCar, green), AliveConstraintViolationException.class);
	}

	public void test001_addInstance_NotAliveException_withMetaRelation() {
		final ClientEngine Engine = new ClientEngine();
		ClientGeneric metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		ClientGeneric car = Engine.addInstance("Car");
		ClientGeneric color = Engine.addInstance("Color");
		final ClientGeneric carColor = Engine.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(metaRelation);
		final ClientGeneric myCar = car.addInstance("myCar");
		final ClientGeneric green = color.addInstance("green");
		myCar.remove();
		assert !myCar.isAlive();
		catchAndCheckCause(() -> carColor.addInstance("myCarColor", myCar, green), AliveConstraintViolationException.class);
	}

	public void test002_addInstance_2composites() {
		final ClientEngine cache = new ClientEngine();
		ClientGeneric car = cache.addInstance("Car");
		ClientGeneric color = cache.addInstance("Color");
		final ClientGeneric carColor = cache.addInstance("CarColor", car, color);
		final ClientGeneric myCar = car.addInstance("myCar");
		final ClientGeneric green = color.addInstance("green");
		assert myCar.isAlive();
		carColor.addInstance("myCarColor", myCar, green);
	}

	public void test002_addInstance_2composites_MetaRelation() {
		final ClientEngine Engine = new ClientEngine();
		ClientGeneric metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		ClientGeneric car = Engine.addInstance("Car");
		ClientGeneric color = Engine.addInstance("Color");
		final ClientGeneric carColor = Engine.addInstance("CarColor", car, color);
		assert carColor.isInstanceOf(metaRelation);
		final ClientGeneric myCar = car.addInstance("myCar");
		final ClientGeneric green = color.addInstance("green");
		assert myCar.isAlive();
		carColor.addInstance("myCarColor", myCar, green);
	}

	public void test003_addInstance_reflexiveRelation() {
		final ClientEngine Engine = new ClientEngine();
		ClientGeneric vehicle = Engine.addInstance("Vehicle");
		ClientGeneric car = vehicle.addInstance("Car");
		ClientGeneric caravane = vehicle.addInstance("Caravane");
		ClientGeneric vehicleHaveSameOwnerAsVehicle = Engine.addInstance("VehicleHaveSameOwnerAsVehicle", vehicle, vehicle);
		ClientGeneric myVehicleHaveSameOwnerAsVehicle = vehicleHaveSameOwnerAsVehicle.addInstance("myVehicleHaveSameOwnerAsVehicle", car, caravane);
		List<ClientGeneric> composites = myVehicleHaveSameOwnerAsVehicle.getComponents();
		assert composites.size() == 2 : composites.size();
		assert composites.contains(caravane) : composites;
		assert composites.contains(car) : composites;
	}

	public void test003_addInstance_reflexiveRelation_MetaRelation() {
		final ClientEngine Engine = new ClientEngine();
		ClientGeneric metaRelation = Engine.setInstance(Engine.getValue(), Engine, Engine);
		ClientGeneric vehicle = Engine.addInstance("Vehicle");
		ClientGeneric car = vehicle.addInstance("Car");
		ClientGeneric caravane = vehicle.addInstance("Caravane");
		ClientGeneric vehicleHaveSameOwnerAsVehicle = Engine.addInstance("VehicleHaveSameOwnerAsVehicle", vehicle, vehicle);
		assert vehicleHaveSameOwnerAsVehicle.isInstanceOf(metaRelation);
		ClientGeneric myVehicleHaveSameOwnerAsVehicle = vehicleHaveSameOwnerAsVehicle.addInstance("myVehicleHaveSameOwnerAsVehicle", car, caravane);
		List<ClientGeneric> composites = myVehicleHaveSameOwnerAsVehicle.getComponents();
		assert composites.size() == 2 : composites.size();
		assert composites.contains(caravane) : composites;
		assert composites.contains(car) : composites;
	}

	public void test004_addInheritsRelation() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		assert carColorMat.inheritsFrom(vehicleColor);
		assert vehicleColor.getInheritings().contains(carColorMat);
		assert vehicleColor.getInheritings().size() == 1;
		assert !carColorMat.isInstanceOf(vehicleColor);
	}

	public void test005_addInheritsRelations_OnSameStructural() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);
		ClientGeneric vehicleColorMat = vehicle.addAttribute(vehicleColor, "vehicleColorMat", color);

		assert vehicleColorMat.inheritsFrom(vehicleColor);
		assert vehicleColor.getInheritings().contains(vehicleColorMat);
		assert vehicleColor.getInheritings().size() == 1;
		assert !vehicleColorMat.isInstanceOf(vehicleColor);
	}

	public void test006_inherits_different_than_instance() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		assert myVehicleRed.isInstanceOf(vehicleColor);
		assert !myVehicleRed.isInstanceOf(carColorMat);
		assert carColorMat.inheritsFrom(vehicleColor);
	}

	public void test007_addInstance_ofSubRelation() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		ClientGeneric myCar = car.addInstance("myCar");
		ClientGeneric redMat = colorMat.addInstance("redMat");
		ClientGeneric myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		assert myCarRedMat.isInstanceOf(carColorMat);
		assert myCarRedMat.isInstanceOf(vehicleColor);
		assert vehicleColor.getInstances().size() == 0;
		assert carColorMat.getInstances().size() == 1;
	}

	public void test008() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		ClientGeneric myCar = car.addInstance("myCar");
		ClientGeneric redMat = colorMat.addInstance("redMat");
		ClientGeneric myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		ClientGeneric vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		assert carColorMat.getAttributes().contains(vehicleColorIsCold);
	}

	public void test009() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		ClientGeneric myCar = car.addInstance("myCar");
		ClientGeneric redMat = colorMat.addInstance("redMat");
		ClientGeneric myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		ClientGeneric vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		assert carColorMat.getAttributes().contains(vehicleColorIsCold);
		assert vehicleColor.getAttributes().contains(vehicleColorIsCold);
	}

	public void test010() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("vehicleColor", color);

		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric colorMat = engine.addInstance(color, "ColorMat");
		ClientGeneric carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		ClientGeneric red = color.addInstance("red");
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		ClientGeneric myCar = car.addInstance("myCar");
		ClientGeneric redMat = colorMat.addInstance("redMat");
		myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		ClientGeneric vehiclePower = vehicle.addAttribute("power");
		ClientGeneric myVehicle125 = myVehicle.addHolder(vehiclePower, "125");

		assert myVehicle.getHolders(vehiclePower).contains(myVehicle125) : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle125.getValue().equals("125");
		assert myVehicle.getValues(vehiclePower).contains("125") : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle.getHolders(vehiclePower).size() == 1;

		assert myCar.getHolders(vehiclePower).size() == 0;

	}
}
