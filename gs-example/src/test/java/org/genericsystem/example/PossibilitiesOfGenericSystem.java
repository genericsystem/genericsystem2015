package org.genericsystem.example;

import java.util.Arrays;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class PossibilitiesOfGenericSystem {
	public void simpleInheriting() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		Generic brand = vehicle.addAttribute("Brand");

		Generic car = engine.addInstance(vehicle, "Car");
		Generic numberOfPassengers = car.addAttribute("NumberOfPassengers");

		Generic truck = engine.addInstance(vehicle, "Truck");
		Generic maximumLoad = truck.addAttribute("MaximumLoad");

		// Add data
		Generic firstCar = car.addInstance("firstCar");
		firstCar.addHolder(brand, "cheetah");
		firstCar.addHolder(numberOfPassengers, 2);

		Generic firstTruck = truck.addInstance("firstTruck");
		firstTruck.addHolder(brand, "yankee");
		firstTruck.addHolder(maximumLoad, 2000);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void multipleInheriting() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		Generic brand = vehicle.addAttribute("Brand");

		Generic human = engine.addInstance("Human");
		Generic name = human.addAttribute("Name");

		Generic transformer = engine.addInstance(Arrays.asList(vehicle, human), "Transformer");

		// Add data
		Generic firstTransformer = transformer.addInstance("firstTransformer");
		firstTransformer.addHolder(brand, "cheetah");
		firstTransformer.addHolder(name, "super500");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void hotStructuralModification() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");

		// Add data
		Generic firstVehicle = vehicle.addInstance("firstVehicle");
		Generic secondVehicle = vehicle.addInstance("secondVehicle");
		Generic thirdVehicle = vehicle.addInstance("thirdVehicle");

		// Modify the structure
		Generic brand = vehicle.addAttribute("Brand");

		// Add new data
		firstVehicle.addHolder(brand, "cheetah");
		secondVehicle.addHolder(brand, "infernus");
		thirdVehicle.addHolder(brand, "phoenix");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void mutability() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");

		// Add data
		vehicle.addInstance("firstVehicle");
		vehicle.addInstance("secondVehicle");
		vehicle.addInstance("thirdVehicle");

		// Update the name of the table
		vehicle.updateValue("VehicleTable");

		// Add new data
		vehicle.addInstance("fourthVehicle");
		vehicle.addInstance("fifthVehicle");

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void defaultLink() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");

		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		Generic white = color.addInstance("white");
		vehicle.addLink(vehicleColor, "defaultWhiteVehicle", white);

		// Add data
		vehicle.addInstance("firstVehicle");

		Generic secondVehicle = vehicle.addInstance("secondVehicle");
		Generic yellow = color.addInstance("yellow");
		secondVehicle.addLink(vehicleColor, "yellowSecondVehicle", yellow);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void queryDatabase() {
		// Create an in memory engine
		Engine engine = new Engine();

		// Create the structure
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");

		Generic color = engine.addInstance("Color");

		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Add data
		// A first Vehicle with a Power of 30 and linked to the Color white
		Generic firstVehicle = vehicle.addInstance("firstVehicle");
		firstVehicle.addHolder(power, 30);
		Generic white = color.addInstance("white");
		firstVehicle.addLink(vehicleColor, "whiteFirstVehicle", white);

		// A second Vehicle with a Power of 55 and linked to the Color white
		Generic secondVehicle = vehicle.addInstance("secondVehicle");
		secondVehicle.addHolder(power, 55);
		secondVehicle.addLink(vehicleColor, "whiteSecondVehicle", white);

		// A third Vehicle with a Power of 50 and linked to the Color yellow
		Generic thirdVehicle = vehicle.addInstance("thirdVehicle");
		thirdVehicle.addHolder(power, 50);
		Generic yellow = color.addInstance("yellow");
		thirdVehicle.addLink(vehicleColor, "yellowThirdVehicle", yellow);

		// A fourth Vehicle with a Power of 89 and linked to the Color white
		Generic fourthVehicle = vehicle.addInstance("fourthVehicle");
		fourthVehicle.addHolder(power, 89);
		fourthVehicle.addLink(vehicleColor, "whiteFourthVehicle", white);

		// A fifth Vehicle with a Power of 120 and linked to the Color red
		Generic fifthVehicle = vehicle.addInstance("fifthVehicle");
		fifthVehicle.addHolder(power, 120);
		Generic red = color.addInstance("red");
		fifthVehicle.addLink(vehicleColor, "redFifthVehicle", red);

		// Query the database
		@SuppressWarnings("unused")
		Snapshot<Generic> searchedVehicles = () -> vehicle.getInstances().stream().filter(
														generic -> generic.getHolders(power).stream().anyMatch(
																holder -> (int) holder.getValue() >= 50 && (int) holder.getValue() <= 90
														)
												   )
												   .filter(
														   generic -> generic.getLinks(vehicleColor).stream().anyMatch(
																   link -> link.getTargetComponent().equals(white)
														   )
												   );

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void staticSetting() {
		// Create an in memory engine specifying parameterized classes
		Engine engine = new Engine(Vehicle.class, Option.class, Color.class, VehicleColor.class);

		// Retrieve our system vehicleManagement
		Vehicle vehicle = engine.find(Vehicle.class);

		// Manage our system vehicleManagement
		Option airConditioning = Option.createOption("air conditioning");
		vehicle.addOption(airConditioning);

		// Persist changes
		engine.getCurrentCache().flush();
	}

	// classes for example staticSetting

	@SystemGeneric
	public static class Vehicle {
		private Option option;

		public Option getOption() {
			return option;
		}

		public void addOption(Option option) {
			this.option = option;
			option.owner = this;
		}

		public void removeOption() {
			option.owner = null;
			option = null;
		}
	}

	@SystemGeneric
	@Components(Vehicle.class)
	public static class Option {
		private String name;

		private Vehicle owner;

		public static Option createOption(String name) {
			Option option = new Option();
			option.name = name;
			return option;
		}

		public String getName() {
			return name;
		}

		public Vehicle getOwner() {
			return owner;
		}
	}

	@SystemGeneric
	public static class Color {
	}

	@SystemGeneric
	@Components({ Vehicle.class, Color.class })
	public static class VehicleColor {
	}
}
