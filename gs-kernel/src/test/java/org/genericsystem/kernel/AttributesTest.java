package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class AttributesTest extends AbstractTest {

	public void test001() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == vehicle.getAttribute("power");
	}

	public void test002() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == vehicle.getAttribute("power");
	}

	public void test003() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == myVehicle.getAttribute("power");
	}

	public void test004() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == car.getAttribute("power");
	}

	public void test005() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myCar = car.addInstance("myCar");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == myCar.getAttribute("power");
	}

	public void test006() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		assert vehicle.getLevel() == 1 : vehicle.getLevel();
		Generic power = root.addInstance("Power", vehicle);
		assert root.getMetaAttribute().getInstance("Power", vehicle) == power;
		assert power.getComponents().size() == 1;
		assert vehicle.equals(power.getComponents().get(0));
		assert power.isAlive();
	}

	public void test007() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic carPower = root.addInstance("Power", car);
		Generic carPowerUnit = root.addInstance("Unit", carPower);
		assert carPower.isDependencyOf(root, Collections.emptyList(), "Power", Collections.singletonList(vehicle));
		assert carPowerUnit.isDependencyOf(root, Collections.emptyList(), "Power", Collections.singletonList(vehicle));
		assert !carPowerUnit.inheritsFrom(root, "Power", Collections.singletonList(vehicle));
		Generic vehiclePower = root.addInstance("Power", vehicle);
		assert root.getMetaAttribute().getInstance("Power", car).getSupers().stream().anyMatch(x -> x.equals(vehiclePower));
	}

	public void test008() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");
		assert !power.getMeta().equals(root);
		assert power.getMeta().equals(root.getMetaAttribute());
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic myVehicle123 = myVehicle.addHolder(power, "myVehicle123");

		assert myVehicle123.isDependencyOf(root, Collections.emptyList(), "Power", Collections.singletonList(vehicle));
		assert !myVehicle123.inheritsFrom(root, "Power", Collections.singletonList(vehicle));
	}

	public void test009() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		catchAndCheckCause(() -> myVehicle.addAttribute("Power", vehicle), MetaRuleConstraintViolationException.class);
	}

	public void test010() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		assert root.getLevel() == 0;
		assert vehicle.getLevel() == 1;
		assert car.getLevel() == 1;
		assert power.getLevel() == 1;
		assert car.getAttributes(root).contains(power);
	}

	public void test011() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic airconditioner = root.addInstance("AirConditioner", vehicle);
		assert vehicle.getAttributes(root).contains(power);
		assert vehicle.getAttributes(root).contains(airconditioner);
		assert power.isAlive();
		assert airconditioner.isAlive();
	}

	public void test012() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic airconditioner = root.addInstance("AirConditioner", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic microcar = root.addInstance(car, "Microcar");
		assert vehicle.getAttributes(root).contains(power);
		assert vehicle.getAttributes(root).contains(airconditioner);
		assert car.getAttributes(root).contains(power);
		assert car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test013() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic airconditioner = root.addInstance("AirConditioner", car);
		Generic microcar = root.addInstance(car, "Microcar");
		assert !vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert car.getAttributes(root).contains(power);
		assert car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test014() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic microcar = root.addInstance(car, "Microcar");
		Generic power = root.addInstance("Power", microcar);
		Generic airconditioner = root.addInstance("AirConditioner", microcar);
		assert !vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert !car.getAttributes(root).contains(power);
		assert !car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test015() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic airconditioner = root.addInstance("AirConditioner", car);
		Generic microcar = root.addInstance(car, "Microcar");
		assert vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert car.getAttributes(root).contains(power);
		assert car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test016() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic microcar = root.addInstance(car, "Microcar");
		Generic airconditioner = root.addInstance("AirConditioner", microcar);
		assert vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert car.getAttributes(root).contains(power);
		assert !car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test017() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic microcar = root.addInstance(car, "Microcar");
		Generic airconditioner = root.addInstance("AirConditioner", microcar);
		assert !vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert car.getAttributes(root).contains(power);
		assert !car.getAttributes(root).contains(airconditioner);
		assert microcar.getAttributes(root).contains(power);
		assert microcar.getAttributes(root).contains(airconditioner);
	}

	public void test018() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(vehicle, "Car");
		Generic caravan = root.addInstance(vehicle, "Caravan");
		assert vehicle.getAttributes(root).contains(power);
		assert car.getAttributes(root).contains(power);
		assert caravan.getAttributes(root).contains(power);
	}

	public void test019() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", car);
		Generic caravan = root.addInstance(vehicle, "Caravan");
		assert !vehicle.getAttributes(root).contains(power);
		assert car.getAttributes(root).contains(power);
		assert !caravan.getAttributes(root).contains(power);
	}

	public void test020() {
		Generic root = new LightServerEngine();
		Generic object = root.addInstance("Object");
		Generic power = root.addInstance("Power", object);
		Generic airconditioner = root.addInstance("AirConditioner", object);
		Generic vehicle = root.addInstance(object, "Vehicle");
		Generic robot = root.addInstance(object, "Robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert object.getAttributes(root).contains(power);
		assert object.getAttributes(root).contains(airconditioner);
		assert vehicle.getAttributes(root).contains(power);
		assert vehicle.getAttributes(root).contains(airconditioner);
		assert robot.getAttributes(root).contains(power);
		assert robot.getAttributes(root).contains(airconditioner);
		assert transformer.getAttributes(root).contains(power);
		assert transformer.getAttributes(root).contains(airconditioner);
	}

	public void test021() {
		Generic root = new LightServerEngine();
		Generic object = root.addInstance("Object");
		Generic vehicle = root.addInstance(object, "Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic airconditioner = root.addInstance("AirConditioner", vehicle);
		Generic robot = root.addInstance(object, "Robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert !object.getAttributes(root).contains(power);
		assert !object.getAttributes(root).contains(airconditioner);
		assert !robot.getAttributes(root).contains(power);
		assert !robot.getAttributes(root).contains(airconditioner);
		assert vehicle.getAttributes(root).contains(power);
		assert vehicle.getAttributes(root).contains(airconditioner);
		assert transformer.getAttributes(root).contains(power);
		assert transformer.getAttributes(root).contains(airconditioner);
	}

	public void test022() {
		Generic root = new LightServerEngine();
		Generic object = root.addInstance("Object");
		Generic vehicle = root.addInstance(object, "Vehicle");
		Generic robot = root.addInstance(object, "Robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic power = root.addInstance("Power", transformer);
		Generic airconditioner = root.addInstance("AirConditioner", transformer);
		assert !object.getAttributes(root).contains(power);
		assert !object.getAttributes(root).contains(airconditioner);
		assert !robot.getAttributes(root).contains(power);
		assert !robot.getAttributes(root).contains(airconditioner);
		assert !vehicle.getAttributes(root).contains(power);
		assert !vehicle.getAttributes(root).contains(airconditioner);
		assert transformer.getAttributes(root).contains(power);
		assert transformer.getAttributes(root).contains(airconditioner);
	}

	/**
	 * Other tests ---------- TODO put in RelationTest
	 */
	public void test023() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic colorVehicle = vehicle.addRelation("colorVehicle", color);
		assert colorVehicle == vehicle.getRelation("colorVehicle", color);
	}

	public void test024() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic ultraColor = root.addInstance(color, "UltraColor");
		Generic ultraColorVehicle = vehicle.addRelation("colorVehicle", ultraColor);
		assert null == vehicle.getRelation("colorVehicle", vehicle, color);
		assert ultraColorVehicle == vehicle.getRelation("colorVehicle", ultraColor);
	}

	public void test025() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic color = root.addInstance("Color");
		Generic ultraColor = root.addInstance(color, "UltraColor");
		vehicle.addRelation("colorVehicle", color);
		Generic ultraColorVehicle = vehicle.addRelation("colorVehicle", ultraColor);
		assert null == myVehicle.getRelation("colorVehicle", vehicle, color);
		assert ultraColorVehicle == myVehicle.getRelation("colorVehicle", ultraColor);
	}

}
