package org.genericsystem.mutability;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ReferentialIntegrityConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class ReferentialIntegrityConstraintTest extends AbstractTest {

	public void test001_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addAttribute("VehicleColor", color);
		catchAndCheckCause(() -> color.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test002_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addAttribute("VehicleColor", color);
		vehicle.remove();
	}

	public void test004_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addAttribute("VehicleColor", color);
		engine.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		vehicle.remove();
	}

	public void test005_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVechile");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);

	}

	public void test005_enableReferentialIntegrity_remove2() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVechile");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);
		vehicleColor.enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> myVehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test005_enableReferentialIntegrity_remove3() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addAttribute("vehicleColor", color);
		engine.getMetaRelation().enableReferentialIntegrity(ApiStatics.BASE_POSITION);
		catchAndCheckCause(() -> vehicle.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test006_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVechile");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);
		myVehicle.remove();
	}

	public void test007_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVechile");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);
		catchAndCheckCause(() -> red.remove(), ReferentialIntegrityConstraintViolationException.class);
	}

	public void test008_enableReferentialIntegrity_remove() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVechile");
		Generic color = engine.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);
		engine.getMetaAttribute().disableReferentialIntegrity(ApiStatics.BASE_POSITION);
		myVehicle.remove();
	}
}
