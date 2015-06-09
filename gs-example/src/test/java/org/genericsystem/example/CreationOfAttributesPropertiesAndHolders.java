package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class CreationOfAttributesPropertiesAndHolders {
	public void createAttribute() {
		Engine engine = new Engine();

		// Create a type vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Add an attribute Options
		vehicle.addAttribute("Options");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createHolder() {
		Engine engine = new Engine();

		// Create a type vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Add an attribute Options
		Generic options = vehicle.addAttribute("Options");

		// Create an instance of type Vehicle
		Generic myVehicle = vehicle.addInstance("myVehicle");

		// Add two Options on myVehicle
		myVehicle.addHolder(options, "music player");
		myVehicle.addHolder(options, "air conditioning");
		// myVehicle has two Options : music player and air conditioning

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createProperty() {
		Engine engine = new Engine();

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Add a property Power
		Generic power = vehicle.addAttribute("Power").enablePropertyConstraint();

		// Create an instance of Vehicle
		Generic myVehicle = vehicle.addInstance("myVehicle");

		// Add a Power on myVehicle
		myVehicle.addHolder(power, 213);
		// myVehicle has one Power : 213

		// Persist changes
		engine.getCurrentCache().flush();

		try {
			// Add another Power on myVehicle
			myVehicle.addHolder(power, 220);
		} catch (Exception e) {
			System.out.println(e);
			// PropertyConstraintViolationException : Power is a property, it can have only one value
		}
	}
}
