package org.genericsystem.remote;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.common.EnginesDeploymentConfig;
import org.genericsystem.common.Generic;
import org.genericsystem.common.EnginesDeploymentConfig.DefaultPathSingleEngineDeployment;
import org.genericsystem.remote.ClientEngine;
import org.testng.annotations.Test;

@Test
public class PersistenceTest extends AbstractTest {

	@Override
	public EnginesDeploymentConfig getDeploymentOptions() {
		return new DefaultPathSingleEngineDeployment(directoryPath, Vehicle.class);
	}

	public void testDefaultConfiguration() {
		ClientEngine root = new ClientEngine();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);

	}

	public void testAnnotType() {
		ClientEngine root = new ClientEngine(Vehicle.class);
		root.getCurrentCache().flush();
		ClientEngine engine = new ClientEngine(Vehicle.class);
		compareGraph(root, engine);
		assert engine.find(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info();
		// root.close();
	}

	public void testAnnotType2() {
		ClientEngine root = new ClientEngine(Vehicle.class);
		root.getCurrentCache().flush();
		ClientEngine engine = new ClientEngine();
		compareGraphWitoutTs(root, engine);
		engine.getCurrentCache().flush();
		engine.close();
		ClientEngine engine2 = new ClientEngine(Vehicle.class);
		compareGraph(root, engine2);
		assert engine2.find(Vehicle.class) instanceof Vehicle : engine2.find(Vehicle.class).info();
	}

	private void compareGraphWitoutTs(Generic persistedNode, Generic readNode) {
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
		ClientEngine root = new ClientEngine();
		root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
		assert null != engine.getInstance("Vehicle");
	}

	public void testHolder() {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.setAttribute("power");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.setHolder(vehiclePower, "123");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
	}

	public void testAddAndRemove() throws InterruptedException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic truck = root.addInstance(vehicle, "Truck");
		assert vehicle.getTs() < truck.getTs();
		car.remove();
		root.getCurrentCache().flush();
		assert vehicle.getTs() < truck.getTs();
		assert vehicle.getBirthTs() == truck.getBirthTs();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
	}

	public void testLink() {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.setAttribute("VehicleColor", color);
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.setHolder(vehicleColor, "myVehicleRed", red);
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
	}

	public void testHeritageMultiple() {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
	}

	public void testHeritageMultipleDiamond() {
		ClientEngine root = new ClientEngine();
		Generic nommable = root.addInstance("Nommable");
		Generic vehicle = root.addInstance(nommable, "Vehicle");
		Generic robot = root.addInstance(nommable, "Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine();
		compareGraph(root, engine);
	}

	public void testTree() {
		ClientEngine root = new ClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic rootTree = tree.addInstance("Root");
		Generic child = tree.addInstance(rootTree, "Child");
		tree.addInstance(rootTree, "Child2");
		tree.addInstance(child, "Child3");
		root.getCurrentCache().flush();
		// root.close();
		compareGraph(root, new ClientEngine());
	}

}
