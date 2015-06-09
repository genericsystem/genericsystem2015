package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class CreationOfRelationsAndLinks {
	public void createRelation() {
		Engine engine = new Engine();

		// Create the types Vehicle and Color
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");

		// Create the relation VehicleColor between Vehicle and Color
		vehicle.addRelation("VehicleColor", color);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createLink() {
		Engine engine = new Engine();

		// Create the types Vehicle and Color
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");

		// Create the relation VehicleColor between Vehicle and Color
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Create an instance of type Vehicle and an instance of type Color
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");

		// Do the link between the instance of the vehicle and the instance of the color
		myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void createTernaryRelation() {
		Engine engine = new Engine();

		// Create the types Vehicle, Color and Time
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic time = engine.addInstance("Time");

		// Create the relation VehicleColorTime between Vehicle, Color and Time
		Generic vehicleColorTime = vehicle.addRelation("VehicleColorTime", color, time);

		// Create an instance of type Vehicle, an instance of type Color and an instance of type Time
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic now = time.addInstance("now");

		// Do the link between the instance of the vehicle, the instance of the color and the instance of the time
		myVehicle.addLink(vehicleColorTime, "myVehicleRedNow", red, now);

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
