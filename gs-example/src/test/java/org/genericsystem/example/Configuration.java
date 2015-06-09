package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class Configuration {
	public void mountDataBase() {
		// Create an engine named myDataBase and which is persistent
		Engine engine = new Engine("myDataBase", System.getenv("HOME") + "/my_directory_path");

		// Create a Vehicle with a Power
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");

		// Instantiate a Vehicle with a Power 233
		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.addHolder(power, 233);

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
