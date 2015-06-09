package org.genericsystem.example;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class AdvancedSearches {
	public void findTypes() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance("Car");
		Generic bike = engine.addInstance("Bike");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the types Vehicle, Car, Bike
		Snapshot<Generic> types = engine.getInstances();
		assert types.size() >= 3;
		assert types.containsAll(Arrays.asList(vehicle, car, bike));
	}

	public void findAttributes() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");
		Generic wheels = vehicle.addAttribute("Wheels");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the attributes Options, Wheels
		Snapshot<Generic> attributes = vehicle.getAttributes();
		assert attributes.size() >= 2;
		assert attributes.containsAll(Arrays.asList(options, wheels));
	}

	public void findRelations() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the relation VehicleColor from the type Vehicle
		Snapshot<Generic> vehicleRelations = vehicle.getRelations();
		assert vehicleRelations.size() >= 1;
		assert vehicleRelations.containsAll(Arrays.asList(vehicleColor));

		// Find the relation VehicleColor from the type Color
		Snapshot<Generic> colorRelations = color.getRelations();
		assert colorRelations.size() >= 1;
		assert colorRelations.containsAll(Arrays.asList(vehicleColor));
	}

	public void findInstances() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic myFirstVehicle = vehicle.addInstance("myFirstVehicle");
		Generic mySecondVehicle = vehicle.addInstance("mySecondVehicle");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the instances of Vehicle
		Snapshot<Generic> instances = vehicle.getInstances();
		assert instances.size() >= 2;
		assert instances.containsAll(Arrays.asList(myFirstVehicle, mySecondVehicle));
	}

	public void findHolders() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic musicPlayer = myVehicle.addHolder(options, "music player");
		Generic airConditioning = myVehicle.addHolder(options, "air conditioning");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the holders of myVehicle for the attribute Options
		Snapshot<Generic> holders = myVehicle.getHolders(options);
		assert holders.size() >= 2;
		assert holders.containsAll(Arrays.asList(musicPlayer, airConditioning));
	}

	public void findLinks() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Persist changes
		engine.getCurrentCache().flush();

		// Find the link myVehicleRed for the relation VehicleColor from myVehicle
		Snapshot<Generic> myVehicleLinks = myVehicle.getLinks(vehicleColor);
		assert myVehicleLinks.size() >= 1;
		assert myVehicleLinks.containsAll(Arrays.asList(myVehicleRed));

		// Find the link myVehicleRed for the relation VehicleColor from red
		Snapshot<Generic> redLinks = red.getLinks(vehicleColor);
		assert redLinks.size() >= 1;
		assert redLinks.containsAll(Arrays.asList(myVehicleRed));
	}

	public void filter() {
		Engine engine = new Engine();

		// Create a type Vehicle with an attribute Options
		Generic vehicle = engine.addInstance("Vehicle");
		Generic options = vehicle.addAttribute("Options");

		// Add three instances of Vehicle
		Generic myFirstVehicle = vehicle.addInstance("myFirstVehicle");
		Generic mySecondVehicle = vehicle.addInstance("mySecondVehicle");
		Generic myThirdVehicle = vehicle.addInstance("myThirdVehicle");

		// Add three values to Options, one for each instance of Vehicle
		myFirstVehicle.addHolder(options, "air conditioning");
		mySecondVehicle.addHolder(options, "music player");
		myThirdVehicle.addHolder(options, "air conditioning");

		// Persist changes
		engine.getCurrentCache().flush();

		// Find all instances of Vehicle with the Options air conditioning
		Snapshot<Generic> instances = () -> vehicle.getInstances().stream().filter(
												generic -> generic.getHolders(options).stream().anyMatch(
														holder -> holder.getValue().equals("air conditioning")
												)
											);

		assert instances.size() >= 2;
		assert instances.containsAll(Arrays.asList(myFirstVehicle, myThirdVehicle));
	}
}
