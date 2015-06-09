package org.genericsystem.kernel;

import java.io.File;
import java.util.Arrays;
import java.util.Collection;
import java.util.Random;

import org.genericsystem.api.core.annotations.SystemGeneric;
import org.testng.annotations.Test;

@Test
public class PersistenceTest extends AbstractTest {

	private final String directoryPath = System.getenv("HOME") + "/test/snapshot_save";

	public void test001() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	public void test002() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		root.addInstance("Vehicle");
		root.close();
		Root root2 = new Root(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, root2);
		assert null != root2.getInstance("Vehicle");
	}

	public void test003() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
		Generic vehicle = root.find(Vehicle.class);
		vehicle.addInstance("myVehicle");
		assert vehicle.getLifeManager().getBirthTs() == LifeManager.TS_SYSTEM;
		assert vehicle.getInstance("myVehicle").getLifeManager().getBirthTs() > vehicle.getLifeManager().getBirthTs();
		assert vehicle.isSystem();
		root.close();

		Root root2 = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle2 = root2.getInstance(Vehicle.class);
		assert vehicle2.getInstance("myVehicle").getLifeManager().getBirthTs() > vehicle2.getLifeManager().getBirthTs();
		assert !vehicle2.isSystem();
		root2.close();

		Root root3 = new Root(Statics.ENGINE_VALUE, snapshot, Vehicle.class);
		Generic vehicle3 = root3.find(Vehicle.class);
		assert vehicle3.getLifeManager().getBirthTs() == LifeManager.TS_SYSTEM;
		assert vehicle3.getInstance("myVehicle").getLifeManager().getBirthTs() > vehicle3.getLifeManager().getBirthTs();
		assert vehicle3.isSystem();
		root3.close();
	}

	public void test004() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.setAttribute("power");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		myVehicle.setHolder(vehiclePower, "123");
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	public void test005() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		root.addInstance(vehicle, "Bike");
		car.remove();
		root.close();
		Root root2 = new Root(Statics.ENGINE_VALUE, snapshot);
		compareGraph(root, root2);
	}

	public void test006() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = car.setAttribute("CarColor", color);
		Generic myCar = car.addInstance("myCar");
		Generic red = color.addInstance("red");
		myCar.setHolder(carColor, "myCarRed", red);
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	public void test007() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic car = root.addInstance("Car");
		Generic robot = root.addInstance("Robot");
		root.addInstance(Arrays.asList(car, robot), "Transformer");
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	public void test008() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic object = root.addInstance("Object");
		Generic car = root.addInstance(object, "Car");
		Generic robot = root.addInstance(object, "Robot");
		root.addInstance(Arrays.asList(car, robot), "Transformer");
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	public void test009() {
		String snapshot = cleanDirectory(directoryPath + new Random().nextInt());
		Root root = new Root(Statics.ENGINE_VALUE, snapshot);
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = vehicle.addInstance("Car");
		Generic electriccar = vehicle.addInstance(car, "Electriccar");
		vehicle.addInstance(car, "Microcar");
		vehicle.addInstance(electriccar, "Hybrid");
		root.close();
		compareGraph(root, new Root(Statics.ENGINE_VALUE, snapshot));
	}

	private static String cleanDirectory(String directoryPath) {
		File file = new File(directoryPath);
		if (file.exists())
			for (File f : file.listFiles())
				f.delete();
		return directoryPath;
	}

	private void compareGraph(Generic persistedNode, Generic readNode) {
		Collection<Generic> persistVisit = persistedNode.getCurrentCache().computeDependencies(persistedNode);
		Collection<Generic> readVisit = readNode.getCurrentCache().computeDependencies(readNode);
		assert persistVisit.size() == readVisit.size() : persistVisit + " \n " + readVisit;
		for (Generic persist : persistVisit) {
			for (Generic read : readVisit)
				if (persist == read)
					assert false : persistVisit + " \n " + readVisit;
		}
		LOOP: for (Generic persist : persistVisit) {
			for (Generic read : readVisit)
				if (persist.genericEquals(read))
					continue LOOP;
			assert false : persistVisit + " \n " + readVisit;
		}
	}

	@SystemGeneric
	public static class Vehicle {

	}

}
