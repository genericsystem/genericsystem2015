package org.genericsystem.cache;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class PersistenceTest extends AbstractTest {

	private final String directoryPath = System.getenv("HOME") + "/test/snapshot_save";

	public void testDefaultConfiguration() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testAnnotType() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
		compareGraph(root, engine);
		assert engine.find(Vehicle.class) instanceof Vehicle : engine.find(Vehicle.class).info();
	}

	public void testAnnotType2() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
		root.getCurrentCache().flush();
		root.close();

		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraphWitoutTs(root, engine);
		engine.getCurrentCache().flush();
		engine.close();

		Engine engine2 = new Engine(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
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
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		root.addInstance("Vehicle");
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
		assert null != engine.getInstance("Vehicle");
	}

	public void testHolder() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.setAttribute("power");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.setHolder(vehiclePower, "123");
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testAddAndRemove() throws InterruptedException {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic truck = root.addInstance(vehicle, "Truck");
		assert vehicle.getTs() < truck.getTs();
		car.remove();
		root.getCurrentCache().flush();
		assert vehicle.getTs() < truck.getTs();
		assert vehicle.getBirthTs() == truck.getBirthTs();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testLink() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.setAttribute("VehicleColor", color);
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.setHolder(vehicleColor, "myVehicleRed", red);
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testHeritageMultiple() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testHeritageMultipleDiamond() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic nommable = root.addInstance("Nommable");
		Generic vehicle = root.addInstance(nommable, "Vehicle");
		Generic robot = root.addInstance(nommable, "Robot");
		root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		root.getCurrentCache().flush();
		root.close();
		Engine engine = new Engine(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, engine);
	}

	public void testTree() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Engine root = new Engine(Statics.ENGINE_VALUE, snapshot);
		Generic tree = root.addInstance("Tree");
		Generic rootTree = tree.addInstance("Root");
		Generic child = tree.addInstance(rootTree, "Child");
		tree.addInstance(rootTree, "Child2");
		tree.addInstance(child, "Child3");
		root.getCurrentCache().flush();
		root.close();
		compareGraph(root, new Engine(Statics.ENGINE_VALUE, snapshot));
	}

	private static String cleanDirectory(String directoryPath) {
		File file = new File(directoryPath);
		if (file.exists())
			for (File f : file.listFiles())
				f.delete();
		return directoryPath;
	}

	private void compareGraph(Generic persistedNode, Generic readNode) {
		List<Generic> persistVisit = new ArrayList<>(persistedNode.getCurrentCache().computeDependencies(persistedNode));
		List<Generic> readVisit = new ArrayList<>(readNode.getCurrentCache().computeDependencies(readNode));
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (int i = 0; i < persistVisit.size(); i++) {
			assert persistVisit.get(i).genericEquals(readVisit.get(i));
			Generic persitedGeneric = persistVisit.get(i);
			Generic readGeneric = readVisit.get(i);
			assert persitedGeneric.getBirthTs() == readGeneric.getBirthTs() : persistVisit.get(i).info() + " " + persitedGeneric.getBirthTs() + "  " + readGeneric.getBirthTs();
			// assert persistLifeManager.getLastReadTs() == readLifeManager.getLastReadTs();
			assert persitedGeneric.getDeathTs() == readGeneric.getDeathTs();
			// assert persistVisit.get(i).getTs() == readVisit.get(i).getTs();
			assert persistVisit.get(i).genericEquals(readVisit.get(i)) : persistVisit.get(i).info() + " " + readVisit.get(i).info();
		}
	}

}
