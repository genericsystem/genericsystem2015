package org.genericsystem.example;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.DefaultConfig.MetaRelation;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class Removes {
	public void removeType() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");

		// Remove the type Vehicle
		vehicle.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void removeAttribute() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");

		// Remove the attribute Options
		options.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void removeRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Remove the relation VehicleColor
		vehicleColor.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void removeInstance() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVehicle");

		// Remove the instance myVehicle
		myVehicle.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void removeHolder() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic musicPlayer = myVehicle.addHolder(options, "music player");
		myVehicle.addHolder(options, "air conditioning");

		// Remove the holder musicPlayer
		musicPlayer.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void removeLink() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Remove the link myVehicleRed
		myVehicleRed.remove();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void dependentRemove() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Remove the type Vehicle
		vehicle.remove();
		assert !vehicle.isAlive();
		assert !vehicleColor.isAlive();
		assert color.isAlive();

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void referentialIntegrity() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addRelation("VehicleColor", color);

		// Enable referential integrity for Vehicle in VehicleColor for the base : Vehicle
		engine.find(MetaRelation.class).enableReferentialIntegrity(ApiStatics.BASE_POSITION);

		// Persist changes
		engine.getCurrentCache().flush();

		try {
			// Remove the type Vehicle
			vehicle.remove();
		} catch (Exception e) {
			// ReferentialIntegrityConstraintViolationException : VehicleColor is Referential Integrity for ancestor Vehicle by composite position : 0
		}
	}

	public void cascadeRemove() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Disable default referential integrity for Vehicle in VehicleColor for the first target : Color
		engine.find(MetaRelation.class).disableReferentialIntegrity(ApiStatics.TARGET_POSITION);

		// Enable cascade remove for Color in VehicleColor
		engine.find(MetaRelation.class).enableCascadeRemove(ApiStatics.TARGET_POSITION);

		// Remove the type Vehicle
		vehicle.remove();
		assert !vehicle.isAlive();
		assert !vehicleColor.isAlive();
		assert !color.isAlive();

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
