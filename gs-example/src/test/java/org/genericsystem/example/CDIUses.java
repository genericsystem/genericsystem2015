package org.genericsystem.example;

import javax.inject.Inject;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.cdi.Engine;
import org.genericsystem.mutability.Generic;

public class CDIUses extends AbstractTest {
	@Inject
	private Engine engine;

	public void staticSetting() {
		// Retrieve annotated classes
		engine.find(Vehicle.class);
		engine.find(Options.class);
		engine.find(Color.class);
		engine.find(VehicleColor.class);
	}

	// classes for example staticSetting

	@SystemGeneric
	public static class Vehicle {
	}

	@SystemGeneric
	@Components(Vehicle.class)
	public static class Options {
	}

	@SystemGeneric
	public static class Color {
	}

	@SystemGeneric
	@Components({ Vehicle.class, Color.class })
	public static class VehicleColor {
	}

	public void simpleExample() {
		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");

		// Add data
		Generic myBmw = vehicle.addInstance("myBmw");
		myBmw.addHolder(power, 233);

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
