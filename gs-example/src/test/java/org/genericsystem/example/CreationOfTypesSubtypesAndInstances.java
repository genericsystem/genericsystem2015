package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class CreationOfTypesSubtypesAndInstances {
	public void createType() {
		// Create an engine which is in memory
		Engine engine = new Engine();

		// Create a type Vehicle
		engine.addInstance("Vehicle");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createInstance() {
		Engine engine = new Engine();

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Create an instance of type Vehicle
		vehicle.addInstance("myVehicle");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createMultiplesInstances() {
		Engine engine = new Engine();

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Create a first instance of type Vehicle
		vehicle.addInstance("myFirstVehicle");
		// Create a second instance of type Vehicle
		vehicle.addInstance("mySecondVehicle");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createSubtypes() {
		Engine engine = new Engine();

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Create the type Car which is a subtype of Vehicle
		Generic car = engine.addInstance(vehicle, "Car");
		// Create the type Bike which is another subtype of Vehicle
		Generic bike = engine.addInstance(vehicle, "Bike");

		// Create an instance of Vehicle
		vehicle.addInstance("myVehicle");

		// Create an instance of Car
		car.addInstance("myCar");
		// Create an instance of Bike
		bike.addInstance("myBike");

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
