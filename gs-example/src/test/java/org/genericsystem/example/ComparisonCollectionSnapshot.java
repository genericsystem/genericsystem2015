package org.genericsystem.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class ComparisonCollectionSnapshot {
	public void createVehicleWithCollection() {
		// (1)
		class Vehicle {
		}
		Vehicle myFirstVehicle = new Vehicle();

		// (2)
		List<Vehicle> instances = new ArrayList<>();
		instances.add(myFirstVehicle);

		// (3)
		assert instances.size() == 1;
		assert instances.contains(myFirstVehicle);

		// (4)
		Vehicle mySecondVehicle = new Vehicle();

		// (5)
		instances.add(mySecondVehicle);

		// (6)
		assert instances.size() == 2;
		assert instances.containsAll(Arrays.asList(myFirstVehicle, mySecondVehicle));
	}

	public void createVehicleWithSnapshot() {
		// (1)
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic myFirstVehicle = vehicle.addInstance("myFirstVehicle");

		// (2)
		Snapshot<Generic> instances = vehicle.getInstances();

		// (3)
		assert instances.size() == 1;
		assert instances.contains(myFirstVehicle);

		// (4)
		Generic mySecondVehicle = vehicle.addInstance("mySecondVehicle");

		// (5)

		// (6)
		assert instances.size() == 2;
		assert instances.containsAll(Arrays.asList(myFirstVehicle, mySecondVehicle));
	}
}
