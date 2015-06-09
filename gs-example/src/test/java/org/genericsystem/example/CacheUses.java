package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class CacheUses {
	public void simpleUse() {
		// Create an in memory engine
		Engine engine = new Engine();
		// A cache is automatically created

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");
		// Add an attribute Power on Vehicle
		Generic power = vehicle.addAttribute("Power");

		// Create an instance of Vehicle
		Generic myVehicle = vehicle.addInstance("myVehicle");
		// Add a value for Power to myVehicle
		myVehicle.addHolder(power, 213);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void mountCache() {
		// Create an in memory engine
		Engine engine = new Engine();
		// A cache is automatically created

		// Create a type Vehicle
		Generic vehicle = engine.addInstance("Vehicle");

		// Add a property Power on Vehicle
		Generic power = vehicle.addAttribute("Power").enablePropertyConstraint();

		// Mount a cache on the current cache
		engine.getCurrentCache().mount();

		// Create an instance of Vehicle
		Generic myVehicle = vehicle.addInstance("myVehicle");

		// Add a value for Power to myVehicle
		myVehicle.addHolder(power, 213);
		// myVehicle has one Power : 213

		try {
			// Add another value for Power to myVehicle
			myVehicle.addHolder(power, 220);
		} catch (Exception e) {
			// PropertyConstraintViolationException : Power is a property, it can have only one value

			// A rollback is performed by Generic System
			// The value for Power and the instance of Vehicle are lost
			// but thanks to the cache we mount, the type Vehicle and the property Power are NOT lost
		}
	}
}
