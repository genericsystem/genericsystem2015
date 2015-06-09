package org.genericsystem.example;

import java.io.Serializable;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class AnnotationsUses {
	public void dynamicSetting() {
		// Create the engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.addAttribute("Options");
		Generic color = engine.addInstance("Color");
		vehicle.addRelation("VehicleColor", color);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void staticSetting() {
		// Create the engine specifying parameterized classes
		Engine engine = new Engine(Vehicle.class, Options.class, Color.class, VehicleColor.class);

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

	public void crud() {
		// Create the engine, specify the user class Phones
		Engine engine = new Engine(Phones.class);

		// Retrieve the type Phones
		Phones phones = engine.find(Phones.class);

		// Add phones
		phones.add("HTC Hero");
		phones.add("Nokia 3210");
		phones.add("Samsung S4");
		phones.add("HTC One");
		phones.add("HTC One");

		// Removes phones
		phones.remove("HTC One");
		phones.remove("HTC One");

		// Persist changes
		engine.getCurrentCache().flush();

		assert phones.size() == 3;
		assert !phones.contains("HTC One");

		// Get the current cache and clear it
		engine.getCurrentCache().clear();

		assert phones.size() == 3;
		assert !phones.contains("HTC One");
	}

	// classes for example crud

	public static interface SimpleCRUD<T extends Serializable> extends Snapshot<T> {
		@Override
		@SuppressWarnings("unchecked")
		default Stream<T> stream() {
			return ((Generic) this).getInstances().stream().map(x -> (T) x.getValue());
		}

		default void add(T value) {
			((Generic) this).setInstance(value);
		}

		default List<T> getValues() {
			return stream().collect(Collectors.toList());
		}

		default boolean remove(T value) {
			for (Generic instance : ((Generic) this).getInstances()) {
				if (Objects.equals(value, instance.getValue())) {
					instance.remove();
					return true;
				}
			}
			return false;
		}
	}

	@SystemGeneric
	public static class Phones implements SimpleCRUD<String> {
	}
}
