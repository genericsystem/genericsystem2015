package org.genericsystem.remote;

import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;

import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

import io.netty.handler.timeout.TimeoutException;
import javafx.collections.ObservableList;

@Test
public class ObservableDefaultTest extends AbstractTest {

	public void basicObservableAttributesTest() throws InterruptedException {
		ClientEngine engine = new ClientEngine();

		Generic vehicle = engine.addInstance("vehicle");

		Generic power = vehicle.addAttribute("power");
		vehicle.addInstance("myBmw");

		Thread.sleep(100);
	}

	public void basicObservableAttributesTest2() throws InterruptedException {
		ClientEngine engine = new ClientEngine();

		Generic vehicle = engine.addInstance("vehicle");
		ObservableList<Generic> vehicleObservableAttributes = vehicle.getAttributes().toObservableList();

		Generic car = engine.addInstance(vehicle, "car");
		ObservableList<Generic> carObservableAttributes = car.getAttributes().toObservableList();

		Generic power = vehicle.addAttribute("power");
		Thread.sleep(100);
		assert carObservableAttributes.contains(power);
		assert vehicleObservableAttributes.contains(power);

		Generic test = vehicle.addAttribute("test");
		Thread.sleep(100);
		assert carObservableAttributes.contains(test);
		assert vehicleObservableAttributes.contains(test);
	}

	public void basicObservableAttributesTest3() throws InterruptedException {
		ClientEngine engine = new ClientEngine();

		Generic vehicle = engine.addInstance("vehicle");

		Generic power = vehicle.addAttribute("power");
		Generic car = engine.addInstance(vehicle, "car");

		ObservableList<Generic> carObservableAttributes = car.getAttributes().toObservableList();

		Thread.sleep(100);

		assert carObservableAttributes.contains(power);

		Generic color = engine.addAttribute("color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Thread.sleep(100);

		assert carObservableAttributes.contains(color);
		assert carObservableAttributes.contains(vehicleColor);
	}

	public void test_relationTest10() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine engine = new ClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic vehiclePower = vehicle.addAttribute("power");
		ObservableList<Generic> myVehicleObservableHolders = myVehicle.getHolders(vehiclePower).toObservableList();
		myVehicleObservableHolders.contains(null);// force compute
		Generic myVehicle125 = myVehicle.addHolder(vehiclePower, "125");

		Thread.sleep(100);

		assert myVehicleObservableHolders.contains(myVehicle125) : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle125.getValue().equals("125");
		// assert myVehicle.getAsyncValues(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains("125") : myVehicle.getHolders(vehiclePower).info();
		assert myVehicleObservableHolders.size() == 1;

		// assert myCar.getAsyncHolders(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;

	}

	// --------------------------

	public void test_holderTest1() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");

		ObservableList<Generic> vehicleObservableHolders = vehicle.getHolders(power).toObservableList();
		vehicleObservableHolders.size();
		ObservableList<Generic> powerObservableInstances = power.getInstances().toObservableList();
		powerObservableInstances.size();
		// ObservableValue<Generic> powerObservableInstance = power.getObservableInstance();
		// powerObservableInstance.getValue();
		ObservableList<Generic> carObservableHolders = car.getHolders(power).toObservableList();
		carObservableHolders.size();

		Thread.sleep(100);

		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, vehicle);
		assert v1.isInstanceOf(power);
		assert vehicleObservableHolders.size() == 1 : vehicle.getHolders(power);
		assert vehicleObservableHolders.contains(v1) : vehicle.getHolders(power);
		assert carObservableHolders.size() == 1 : vehicle.getHolders(power);
		assert carObservableHolders.contains(v1) : vehicle.getHolders(power);
		assert powerObservableInstances.size() == 1;
		assert powerObservableInstances.contains(v1);
		// assert powerObservableInstance.getValue().equals(v1);
		assert power.isAlive();
		assert v1.isAlive();
	}

	public void test_IteratorAndRemoveTest1() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new ClientEngine();
		Generic car = root.addInstance("Car");
		ObservableList<Generic> myCars = car.getInstances().toObservableList();

		myCars.size();

		car.addInstance("myFirstCar");
		car.addInstance("mySecondCar");
		Generic myThirdCar = car.addInstance("myThirdCar");
		car.addInstance("myFourthCar");

		// TODO: Make modification possible again ?
		// Iterator<Generic> iterator = myCars.iterator();
		Iterator<Generic> iterator = car.getInstances().iterator();
		int cpt = 0;
		myThirdCar.remove();
		Thread.sleep(100);
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3 : cpt;
	}

	public void test_getInstanceTest13() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");

		ObservableList<Generic> vehiclePowerObservableInstances = vehiclePower.getInstances(116).toObservableList();
		vehiclePowerObservableInstances.size();

		Generic car = root.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehiclePower.addInstance(115, myBmw);
		vehiclePower.addInstance(116, myBmw);
		vehiclePower.addInstance(116, myAudi);

		assert vehiclePowerObservableInstances.size() == 2;
	}

	public void test_getInstanceTest14() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		ObservableList<Generic> vehicleColorObservableInstances = vehicleColor.getInstances("").toObservableList();
		vehicleColorObservableInstances.size();

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		Thread.sleep(100);

		assert vehicleColorObservableInstances.size() == 3;
	}

	public void test_getInstanceTest15() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic tree = root.addInstance("Tree");

		ObservableList<Generic> children2ObservableInstances = tree.getInstances("children2").toObservableList();
		children2ObservableInstances.size();

		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		Thread.sleep(100);

		assert children2ObservableInstances.size() == 2;
		assert children2ObservableInstances.containsAll(Arrays.asList(children2, children3));
	}

	public void test_getInstanceTest16() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		ObservableList<Generic> myBmwObservableInstances = vehicleColor.getInstances("", myBmw).toObservableList();
		myBmwObservableInstances.size();

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		ObservableList<Generic> myBmwRedObservableInstances = vehicleColor.getInstances("", myBmw, red).toObservableList();
		myBmwObservableInstances.size();

		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		Thread.sleep(100);

		assert myBmwObservableInstances.size() == 2;
		assert myBmwRedObservableInstances.size() == 1;
	}

	public void test_getInstanceTest17() throws InterruptedException, ExecutionException, TimeoutException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");

		ObservableList<Generic> blueObservableInstances = vehicleColor.getInstances(blue).toObservableList();
		blueObservableInstances.size();
		ObservableList<Generic> myBmwObservableInstances = vehicleColor.getInstances(myBmw).toObservableList();
		myBmwObservableInstances.size();

		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		Thread.sleep(100);

		assert myBmwObservableInstances.size() == 2;
		assert blueObservableInstances.size() == 1;
	}

	public void test_getInstanceTest20() throws InterruptedException {
		ClientEngine root = new ClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myBmw = vehicle.addInstance("myBmw");
		ObservableList<Generic> myBmwObservableSubInstancesByVehicle = vehicle.getSubInstances("myBmw").toObservableList();
		myBmwObservableSubInstancesByVehicle.size();

		vehicle.addInstance("myAudi");
		Generic car = root.addInstance(vehicle, "Car");
		ObservableList<Generic> myBmwObservableSubInstancesByCar = car.getSubInstances("myBmw").toObservableList();
		myBmwObservableSubInstancesByCar.size();

		Generic myBmwCar = car.addInstance("myBmw");
		car.addInstance("myAudi");

		Thread.sleep(100);

		myBmwObservableSubInstancesByVehicle.size();
		myBmwObservableSubInstancesByCar.size();

		assert myBmwObservableSubInstancesByVehicle.containsAll(Arrays.asList(myBmw, myBmwCar)) : vehicle.getSubInstances("myBmw").info();
		assert myBmwObservableSubInstancesByCar.get(0) == myBmwCar;
	}

	public void test_getInstanceTest21() throws InterruptedException, ExecutionException, TimeoutException {//
		ClientEngine root = new ClientEngine();

		Generic tree = root.addInstance("Tree");
		ObservableList<Generic> treeObservableSubInstances = tree.getSubInstances("children2").toObservableList();
		treeObservableSubInstances.size();

		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		assert treeObservableSubInstances.size() == 2;
		assert treeObservableSubInstances.containsAll(Arrays.asList(children2, children3));
	}

	//

	//

	//

	//

	public void test_BindingServiceTest3() throws InterruptedException, ExecutionException, TimeoutException {
		Generic engine = new ClientEngine();
		ObservableList<Generic> engineSubInheritings = engine.getSubInheritings().toObservableList();
		engineSubInheritings.size();

		Generic animal = engine.addInstance("Animal");// Alone type
		ObservableList<Generic> animalSubInheritings = animal.getSubInheritings().toObservableList();
		animalSubInheritings.size();

		Generic machine = engine.addInstance("Machine");
		ObservableList<Generic> machineSubInheritings = machine.getSubInheritings().toObservableList();
		machineSubInheritings.size();

		Generic vehicle = engine.addInstance(machine, "Vehicle");
		ObservableList<Generic> vehicleSubInheritings = vehicle.getSubInheritings().toObservableList();
		vehicleSubInheritings.size();

		Generic robot = engine.addInstance(machine, "Robot");
		ObservableList<Generic> robotSubInheritings = robot.getSubInheritings().toObservableList();
		robotSubInheritings.size();

		Generic car = engine.addInstance(vehicle, "Car");
		ObservableList<Generic> carSubInheritings = car.getSubInheritings().toObservableList();
		carSubInheritings.size();

		Generic bike = engine.addInstance(vehicle, "Bike");
		ObservableList<Generic> bikeSubInheritings = bike.getSubInheritings().toObservableList();
		bikeSubInheritings.size();

		Generic transformer = engine.addInstance(Arrays.asList(robot, car), "Transformer");
		ObservableList<Generic> transformerSubInheritings = transformer.getSubInheritings().toObservableList();
		transformerSubInheritings.size();

		Generic plasticTransformer = engine.addInstance(transformer, "PlasticTransformer");
		ObservableList<Generic> plasticTransformerSubInheritings = plasticTransformer.getSubInheritings().toObservableList();
		plasticTransformerSubInheritings.size();

		Thread.sleep(700);

		engineSubInheritings.size();

		assert engine.getSubInheritings().toList().containsAll(engineSubInheritings);
		assert engine.getSubInheritings().size() == engineSubInheritings.size();

		assert animal.getSubInheritings().toList().containsAll(animalSubInheritings);
		assert animal.getSubInheritings().size() == animalSubInheritings.size();

		assert machine.getSubInheritings().toList().containsAll(machineSubInheritings);
		assert machine.getSubInheritings().size() == machineSubInheritings.size();

		assert vehicle.getSubInheritings().toList().containsAll(vehicleSubInheritings);

		assert vehicle.getSubInheritings().size() == vehicleSubInheritings.size();

		assert robot.getSubInheritings().toList().containsAll(robotSubInheritings);
		assert robot.getSubInheritings().size() == robotSubInheritings.size();

		assert car.getSubInheritings().toList().containsAll(carSubInheritings);
		assert car.getSubInheritings().size() == carSubInheritings.size();

		assert bike.getSubInheritings().toList().containsAll(bikeSubInheritings);
		assert bike.getSubInheritings().size() == bikeSubInheritings.size();

		assert transformer.getSubInheritings().toList().containsAll(transformerSubInheritings);
		assert transformer.getSubInheritings().size() == transformerSubInheritings.size();

		assert plasticTransformer.getSubInheritings().toList().containsAll(plasticTransformerSubInheritings);
		assert plasticTransformer.getSubInheritings().size() == plasticTransformerSubInheritings.size();

		assert !machineSubInheritings.contains(animal) : machine.getSubInheritings().info();
		assert machineSubInheritings.containsAll(Arrays.asList(machine, vehicle, robot, car, bike, transformer, plasticTransformer)) : machine.getSubInheritings().info();
		assert machineSubInheritings.size() == 7 : machine.getSubInheritings().info();
	}

	public void test_getInstanceTest24() throws InterruptedException {
		ClientEngine root = new ClientEngine();

		ObservableList<Generic> rootSubInstancesPower = root.getRoot().getMetaAttribute().getSubInstances(Collections.emptyList(), "power").toObservableList();

		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");

		ObservableList<Generic> rootSubInstancesPowerOverrideVehiclePower = root.getRoot().getMetaAttribute().getSubInstances(vehiclePower, "power").toObservableList();

		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		Thread.sleep(100);

		assert rootSubInstancesPower.size() == 1;
		assert rootSubInstancesPower.get(0) == vehiclePower;
		assert rootSubInstancesPowerOverrideVehiclePower.size() == 2;
		assert rootSubInstancesPowerOverrideVehiclePower.containsAll(Arrays.asList(bikePower, trunckPower));
	}
}
