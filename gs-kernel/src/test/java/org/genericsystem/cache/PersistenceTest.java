package org.genericsystem.cache;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class PersistenceTest extends AbstractClassicTest {

	public void testDefaultConfiguration() {

		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);

	}

	public void testAnnotType() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		compareGraph(root, engine);
		assert engine.find(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info();
	}

	public void testAnnotType2() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraphWitoutTs(root, engine);
		engine.getCurrentCache().flush();
		engine.close();
		ClientEngine engine2 = new ClientEngine(Statics.ENGINE_VALUE, Vehicle.class);
		compareGraph(root, engine2);
		assert engine2.find(Vehicle.class) instanceof Vehicle : engine2.find(Vehicle.class).info();
	}

	private void compareGraphWitoutTs(ClientGeneric persistedNode, ClientGeneric readNode) {
		List<ClientGeneric> persistVisit = new ArrayList<>(persistedNode.getCurrentCache().computeDependencies(persistedNode));
		List<ClientGeneric> readVisit = new ArrayList<>(readNode.getCurrentCache().computeDependencies(readNode));
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (int i = 0; i < persistVisit.size(); i++) {
			assert persistVisit.get(i).genericEquals(readVisit.get(i));
		}
	}

	@SystemGeneric
	public static class Vehicle implements ClientGeneric {
	}

	public void testType() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
		assert null != engine.getInstance("Vehicle");
	}

	public void testHolder() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric vehicle = root.addInstance("Vehicle");
		ClientGeneric vehiclePower = vehicle.setAttribute("power");
		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.setHolder(vehiclePower, "123");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testAddAndRemove() throws InterruptedException {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric vehicle = root.addInstance("Vehicle");
		ClientGeneric car = root.addInstance(vehicle, "Car");
		ClientGeneric truck = root.addInstance(vehicle, "Truck");
		assert vehicle.getTs() < truck.getTs();
		car.remove();
		root.getCurrentCache().flush();
		assert vehicle.getTs() < truck.getTs();
		assert vehicle.getBirthTs() == truck.getBirthTs();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testLink() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric vehicle = root.addInstance("Vehicle");
		ClientGeneric color = root.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.setAttribute("VehicleColor", color);
		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		ClientGeneric red = color.addInstance("red");
		myVehicle.setHolder(vehicleColor, "myVehicleRed", red);
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testHeritageMultiple() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric vehicle = root.addInstance("Vehicle");
		ClientGeneric robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testHeritageMultipleDiamond() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric nommable = root.addInstance("Nommable");
		ClientGeneric vehicle = root.addInstance(nommable, "Vehicle");
		ClientGeneric robot = root.addInstance(nommable, "Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		// root.close();
		ClientEngine engine = new ClientEngine(Statics.ENGINE_VALUE);
		compareGraph(root, engine);
	}

	public void testTree() {
		ClientEngine root = new ClientEngine(Statics.ENGINE_VALUE);
		ClientGeneric tree = root.addInstance("Tree");
		ClientGeneric rootTree = tree.addInstance("Root");
		ClientGeneric child = tree.addInstance(rootTree, "Child");
		tree.addInstance(rootTree, "Child2");
		tree.addInstance(child, "Child3");
		root.getCurrentCache().flush();
		// root.close();
		compareGraph(root, new ClientEngine(Statics.ENGINE_VALUE));
	}

	private void compareGraph(ClientGeneric persistedNode, ClientGeneric readNode) {
		List<ClientGeneric> persistVisit = new ArrayList<>(persistedNode.getCurrentCache().computeDependencies(persistedNode));
		List<ClientGeneric> readVisit = new ArrayList<>(readNode.getCurrentCache().computeDependencies(readNode));
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (int i = 0; i < persistVisit.size(); i++) {
			assert persistVisit.get(i).genericEquals(readVisit.get(i));
			ClientGeneric persitedGeneric = persistVisit.get(i);
			ClientGeneric readGeneric = readVisit.get(i);
			assert persitedGeneric.getBirthTs() == readGeneric.getBirthTs() : persistVisit.get(i).info() + " " + persitedGeneric.getBirthTs() + "  " + readGeneric.getBirthTs();
			// assert persistLifeManager.getLastReadTs() == readLifeManager.getLastReadTs();
			// assert persitedGeneric.getDeathTs() == readGeneric.getDeathTs();
			// assert persistVisit.get(i).getTs() == readVisit.get(i).getTs();
			assert persistVisit.get(i).genericEquals(readVisit.get(i)) : persistVisit.get(i).info() + " " + readVisit.get(i).info();
		}
	}

}
