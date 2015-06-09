package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class BasicSearches {
	public void findType() {
		Engine engine = new Engine();

		engine.addInstance("Vehicle");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the type Vehicle
		engine.getInstance("Vehicle");
	}

	public void findAttribute() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.addAttribute("Options");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the attribute Options
		vehicle.getAttribute("Options");
	}

	public void findRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		vehicle.addRelation("VehicleColor", color);

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the relation VehicleColor from the type Vehicle
		vehicle.getRelation("VehicleColor");

		// Find the relation VehicleColor from the type Color
		color.getRelation("VehicleColor");
	}

	public void findInstance() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.addInstance("myVehicle");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the instance myVehicle
		vehicle.getInstance("myVehicle");
	}

	public void findHolder() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");

		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.addHolder(options, "music player");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the holder music player for the attribute Options
		myVehicle.getHolder(options, "music player");
	}

	public void findLink() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the link myVehicleRed for the relation VehicleColor from myVehicle
		myVehicle.getLink(vehicleColor, "myVehicleRed");

		// Find the link myVehicleRed for the relation VehicleColor from red
		red.getLink(vehicleColor, "myVehicleRed");
	}
}
