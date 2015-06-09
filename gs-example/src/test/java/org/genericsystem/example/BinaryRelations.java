package org.genericsystem.example;

import java.util.Arrays;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class BinaryRelations {
	public void oneToOneRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Make VehicleColor a 1-1 relation
		vehicleColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		vehicleColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic yourVehicle = vehicle.addInstance("yourVehicle");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		// Create the link between myVehicle and red from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();

		try {
			// Create the link between myVehicle and yellow from the relation VehicleColor
			myVehicle.addLink(vehicleColor, "myVehicleYellow", yellow);
		} catch (Exception e) {
			// SingularConstraintViolationException : myVehicle has more than one link : [myVehicleRed, myVehicleYellow] for relation VehicleColor
		}

		try {
			// Create the link between yourVehicle and red from the relation VehicleColor
			yourVehicle.addLink(vehicleColor, "yourVehicleRed", red);
		} catch (Exception e) {
			// SingularConstraintViolationException : red has more than one link : [myVehicleRed, yourVehicleRed] for relation VehicleColor
		}
	}

	public void oneToManyRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Make VehicleColor a 1-n relation
		vehicleColor.enableSingularConstraint(ApiStatics.BASE_POSITION);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic yourVehicle = vehicle.addInstance("yourVehicle");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		// Create the link between myVehicle and red from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();

		try {
			// Create the link between myVehicle and yellow from the relation VehicleColor
			myVehicle.addLink(vehicleColor, "myVehicleYellow", yellow);
		} catch (Exception e) {
			// SingularConstraintViolationException : myVehicle has more than one link : [myVehicleRed, myVehicleYellow] for relation VehicleColor
		}

		// Create the link between yourVehicle and red from the relation VehicleColor
		yourVehicle.addLink(vehicleColor, "yourVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void manyToOneRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Make VehicleColor a n-1 relation
		vehicleColor.enableSingularConstraint(ApiStatics.TARGET_POSITION);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic yourVehicle = vehicle.addInstance("yourVehicle");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		// Create the link between myVehicle and red from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();

		// Create the link between myVehicle and yellow from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleYellow", yellow); // OK

		// Persist changes
		engine.getCurrentCache().flush();

		try {
			// Create the link between yourVehicle and red from the relation VehicleColor
			yourVehicle.addLink(vehicleColor, "yourVehicleRed", red);
		} catch (Exception e) {
			// SingularConstraintViolationException : red has more than one link : [myVehicleRed, yourVehicleRed] for relation VehicleColor
		}
	}

	public void manyToManyRelation() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Make VehicleColor a n-m relation : nothing to do, it is the case by default

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic yourVehicle = vehicle.addInstance("yourVehicle");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		// Create the link between myVehicle and red from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();

		// Create the link between myVehicle and yellow from the relation VehicleColor
		myVehicle.addLink(vehicleColor, "myVehicleYellow", yellow); // OK

		// Create the link between yourVehicle and red from the relation VehicleColor
		yourVehicle.addLink(vehicleColor, "yourVehicleRed", red); // OK

		// Persist changes
		engine.getCurrentCache().flush();
	}

	public void manyToManyRelationAndSnapshot() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);

		// Make VehicleColor a n-m relation : nothing to do, it is the case by default

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic yourVehicle = vehicle.addInstance("yourVehicle");

		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		// Create links from the base
		Generic myVehicleYellow = myVehicle.addLink(vehicleColor, "myVehicleYellow", yellow);

		// Create links from the target
		// Generic yourVehicleRed = red.addLink(vehicleColor, "yourVehicleRed", yourVehicle);
		Generic yourVehicleRed = yourVehicle.addLink(vehicleColor, "yourVehicleRed", red);

		// Find links from the base
		Snapshot<Generic> linksFromTheBase = myVehicle.getLinks(vehicleColor);
		assert linksFromTheBase.size() == 1;
		assert linksFromTheBase.containsAll(Arrays.asList(myVehicleYellow));

		// Find links from the target
		Snapshot<Generic> linksFromTheTarget = red.getLinks(vehicleColor);
		assert linksFromTheTarget.size() == 1;
		assert linksFromTheTarget.containsAll(Arrays.asList(yourVehicleRed));

		// Add a link from the base
		Generic myVehicleRed = myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Links from the base have been modified and are aware of these changes...
		assert linksFromTheBase.size() == 2;
		assert linksFromTheBase.containsAll(Arrays.asList(myVehicleYellow, myVehicleRed));

		// ...but links from the target have been modified too and are aware of these changes too
		assert linksFromTheTarget.size() == 2;
		assert linksFromTheTarget.containsAll(Arrays.asList(yourVehicleRed, myVehicleRed));

		// Test the same thing but in the other side
		// Remove the last link
		myVehicleRed.remove();
		assert !myVehicleRed.isAlive();

		assert linksFromTheBase.size() == 1;
		assert linksFromTheBase.containsAll(Arrays.asList(myVehicleYellow));

		assert linksFromTheTarget.size() == 1;
		assert linksFromTheTarget.containsAll(Arrays.asList(yourVehicleRed));

		// Add a link from the target
		// myVehicleRed = red.addLink(vehicleColor, "myVehicleRed", myVehicle);
		myVehicleRed = myVehicle.addLink(vehicleColor, "myVehicleRed", red);

		// Links from the target have been modified and are aware of these changes...
		assert linksFromTheTarget.size() == 2;
		assert linksFromTheTarget.containsAll(Arrays.asList(yourVehicleRed, myVehicleRed));

		// ...but links from the base have been modified too and are aware of these changes too
		assert linksFromTheBase.size() == 2;
		assert linksFromTheBase.containsAll(Arrays.asList(myVehicleYellow, myVehicleRed));

		// Persist changes
		engine.getCurrentCache().flush();
	}
}
