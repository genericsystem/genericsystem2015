package org.genericsystem.distributed.cacheonserver;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.GSDeploymentOptions;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class PersistenceTest extends AbstractTest {

	@Override
	public GSDeploymentOptions getDeploymentOptions() {
		return new GSDeploymentOptions().addEngine(Statics.ENGINE_VALUE, directoryPath).addClasses(Vehicle.class);
	}

	public void testDefaultConfiguration() {

		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);

	}

	public void testAnnotType() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		root.getCurrentCache().flush();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		compareGraph(root, engine);
		assert engine.find(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info();
		// root.close();
	}

	public void testAnnotType2() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		root.getCurrentCache().flush();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraphWithoutTs(root, engine);
		engine.getCurrentCache().flush();
		engine.close();
		LightClientEngine engine2 = new LightClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		compareGraphWithoutTs(root, engine2);
		assert engine2.find(Vehicle.class) instanceof Vehicle : engine2.find(Vehicle.class).info();
	}

	private void compareGraphWithoutTs(Generic persistedNode, Generic readNode) {
		List<Generic> persistVisit = new ArrayList<>(persistedNode.getCurrentCache().computeDependencies(persistedNode));
		List<Generic> readVisit = new ArrayList<>(readNode.getCurrentCache().computeDependencies(readNode));
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (int i = 0; i < persistVisit.size(); i++) {
			assert persistVisit.get(i).genericEquals(readVisit.get(i));
		}
	}

	@SystemGeneric
	public static class Vehicle implements Generic {
	}

	public void testType() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
		assert null != engine.getInstance("Vehicle");
	}

	public void testHolder() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.setAttribute("power");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.setHolder(vehiclePower, "123");
		root.getCurrentCache().flush();
		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testAddAndRemove() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic truck = root.addInstance(vehicle, "Truck");
		assert vehicle.getTs() < truck.getTs();
		car.remove();
		root.getCurrentCache().flush();

		assert vehicle.getTs() < truck.getTs();
		assert vehicle.getBirthTs() == truck.getBirthTs();

		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testLink() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.setAttribute("VehicleColor", color);
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.setHolder(vehicleColor, "myVehicleRed", red);
		root.getCurrentCache().flush();
		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testHeritageMultiple() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testHeritageMultipleDiamond() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic nommable = root.addInstance("Nommable");
		Generic vehicle = root.addInstance(nommable, "Vehicle");
		Generic robot = root.addInstance(nommable, "Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		LightClientEngine engine = new LightClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testTree() {
		LightClientEngine root = new LightClientEngine(Statics.ENGINE_VALUE);
		Generic tree = root.addInstance("Tree");
		Generic rootTree = tree.addInstance("Root");
		Generic child = tree.addInstance(rootTree, "Child");
		tree.addInstance(rootTree, "Child2");
		tree.addInstance(child, "Child3");
		root.getCurrentCache().flush();
		// root.close();
		compareGraph(root, new LightClientEngine(Statics.ENGINE_VALUE));
	}
	//
	// private void compareGraph(Generic persistedNode, Generic readNode) {
	//
	// List<Generic> persistVisit = new ArrayList<>(persistedNode.getCurrentCache().computeDependencies(persistedNode));
	// List<Generic> readVisit = new ArrayList<>(readNode.getCurrentCache().computeDependencies(readNode));
	// assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
	// for (int i = 0; i < persistVisit.size(); i++) {
	// assert persistVisit.get(i).genericEquals(readVisit.get(i));
	// Generic persitedGeneric = persistVisit.get(i);
	// Generic readGeneric = readVisit.get(i);
	//
	// assert persitedGeneric.getBirthTs() == readGeneric.getBirthTs() : persistVisit.get(i).info() + " " + persitedGeneric.getBirthTs() + "  " + readGeneric.getBirthTs();
	// // assert persistLifeManager.getLastReadTs() == readLifeManager.getLastReadTs();
	// // assert persitedGeneric.getDeathTs() == readGeneric.getDeathTs();
	// // assert persistVisit.get(i).getTs() == readVisit.get(i).getTs();
	// assert persistVisit.get(i).genericEquals(readVisit.get(i)) : persistVisit.get(i).info() + " " + readVisit.get(i).info();
	// }
	// }

}
