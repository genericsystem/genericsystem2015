package org.genericsystem.distributed.cacheonclient;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class AsyncDefaultTest extends AbstractTest {

	public void test_holderTest1() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, vehicle);
		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);
		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert car.getHolders(power).contains(v1) : vehicle.getHolders(power);
		assert power.getAsyncInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) != null;
		assert power.getAsyncInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert power.getAsyncInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v1);
		assert power.getAsyncInstance(powerValue, vehicle).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).equals(v1);
		assert power.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert v1.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_IteratorAndRemoveTest1() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		car.addInstance("myFirstCar");
		car.addInstance("mySecondCar");
		Generic myThirdCar = car.addInstance("myThirdCar");
		car.addInstance("myFourthCar");

		Snapshot<Generic> myCars = car.getAsyncInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);

		Iterator<Generic> iterator = myCars.iterator();
		int cpt = 0;
		myThirdCar.remove();
		while (iterator.hasNext()) {
			iterator.next();
			cpt++;
		}
		assert cpt == 3;
	}

	public void test_getInstanceTest9() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance("Car");
		Generic carVehicle = root.addInstance(vehicle, "Car");

		Generic carPower = car.addAttribute("power");
		Generic carVehiclePower = carVehicle.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert carVehiclePower.inheritsFrom(vehiclePower);

		catchAndCheckCausePromise(root.getRoot().getMetaAttribute().getAsyncInstance("power"), AmbiguousSelectionException.class);
		// computeAsyncAndCheckOverridesAreReached unimplemented
		// catchAndCheckCausePromise(root.getRoot().getMetaAttribute().getAsyncInstance(Collections.emptyList(), "power"), AmbiguousSelectionException.class);
	}

	public void test_getInstanceTest13() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");

		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehiclePower.addInstance(115, myBmw);
		vehiclePower.addInstance(116, myBmw);
		vehiclePower.addInstance(116, myAudi);

		assert vehiclePower.getAsyncInstances(116).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
	}

	public void test_getInstanceTest14() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getAsyncInstances("").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 3;
	}

	public void test_getInstanceTest15() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		assert tree.getAsyncInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert tree.getAsyncInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).containsAll(Arrays.asList(children2, children3));
	}

	public void test_getInstanceTest16() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getAsyncInstances("", myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert vehicleColor.getAsyncInstances("", myBmw, red).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
	}

	public void test_getInstanceTest17() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getInstances(myBmw).toList().equals(vehicleColor.getAsyncInstances(myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert vehicleColor.getAsyncInstances(myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert vehicleColor.getInstances(blue).toList().equals(vehicleColor.getAsyncInstances(blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert vehicleColor.getAsyncInstances(blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
	}

	public void test_getInstanceTest20() {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myBmw = vehicle.addInstance("myBmw");
		vehicle.addInstance("myAudi");
		Generic car = root.addInstance(vehicle, "Car");
		Generic myBmwCar = car.addInstance("myBmw");
		car.addInstance("myAudi");

		assert vehicle.getSubInstances("myBmw").containsAll(Arrays.asList(myBmw, myBmwCar)) : vehicle.getSubInstances("myBmw").info();
		assert car.getSubInstances("myBmw").first() == myBmwCar;
	}

	public void test_getInstanceTest21() throws InterruptedException, ExecutionException, TimeoutException {//
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		Generic children3 = tree.addInstance(children1, "children2");

		assert tree.getAsyncSubInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert tree.getAsyncSubInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().containsAll(Arrays.asList(children2, children3));// .toList()
	}

	public void test_getInstanceTest22() throws InterruptedException, ExecutionException, TimeoutException {//
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("", myBmw, blue);

		assert vehicleColor.getAsyncSubInstances("", myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert vehicleColor.getAsyncSubInstances("", myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().containsAll(Arrays.asList(myBmwRed, myBmwBlue));// .toList()
	}

	public void test_getInstanceTest25() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		// assert tree.getAsyncSubInstances(Arrays.asList(mother), "children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1; // computeAsyncAndCheckOverridesAreReached unimplemented
		assert tree.getAsyncSubInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().get(0) == children2;// .toList().get(0), before .first()
	}

	public void test_MetaAttribueTest1() {
		CocClientEngine root = new CocClientEngine();
		Generic metaAttribute = root.getMetaAttribute();
		assert metaAttribute != null;
		assert root.getLevel() == 0;
		assert metaAttribute == root.setInstance(root.getValue(), root);
		assert metaAttribute.getLevel() == 0;
		assert metaAttribute.isMeta();
		assert metaAttribute.getMeta() == metaAttribute;
		assert metaAttribute.inheritsFrom(root) : metaAttribute.info();
		assert metaAttribute.isInstanceOf(root);
		assert metaAttribute.isInstanceOf(metaAttribute);
		assert !root.getInstances().contains(metaAttribute);
		assert !root.getSubInstances().contains(metaAttribute);
		assert metaAttribute.getBaseComponent().equals(root);
	}

	public void test_BindingServiceTest3() throws InterruptedException, ExecutionException, TimeoutException {
		Generic engine = new CocClientEngine();
		Generic animal = engine.addInstance("Animal");// Alone type
		Generic machine = engine.addInstance("Machine");
		Generic vehicle = engine.addInstance(machine, "Vehicle");
		Generic robot = engine.addInstance(machine, "Robot");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic bike = engine.addInstance(vehicle, "Bike");
		Generic transformer = engine.addInstance(Arrays.asList(robot, car), "Transformer");
		Generic plasticTransformer = engine.addInstance(transformer, "PlasticTransformer");

		assert engine.getSubInheritings().toList().equals(engine.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert animal.getSubInheritings().toList().equals(animal.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert machine.getSubInheritings().toList().equals(machine.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert vehicle.getSubInheritings().toList().equals(vehicle.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert robot.getSubInheritings().toList().equals(robot.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert car.getSubInheritings().toList().equals(car.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert bike.getSubInheritings().toList().equals(bike.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert transformer.getSubInheritings().toList().equals(transformer.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert plasticTransformer.getSubInheritings().toList().equals(plasticTransformer.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());

		assert !machine.getSubInheritings().contains(animal) : machine.getSubInheritings().info();
		assert machine.getSubInheritings().containsAll(Arrays.asList(machine, vehicle, robot, car, bike, transformer, plasticTransformer)) : machine.getSubInheritings().info();
		assert machine.getSubInheritings().size() == 7 : machine.getSubInheritings().info();

	}

	public void test_BindingServiceTest4() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("tree");
		Generic html = tree.addInstance("html");
		Generic head = tree.addInstance(html, "head");
		Generic body = tree.addInstance(html, "body");
		Generic div = tree.addInstance(body, "div");

		assert tree.getSubInheritings().toList().equals(tree.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert html.getSubInheritings().toList().equals(html.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert head.getSubInheritings().toList().equals(head.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert body.getSubInheritings().toList().equals(body.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert div.getSubInheritings().toList().equals(div.getAsyncSubInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());

		assert !html.getInheritings().contains(html);
		assert html.getInheritings().containsAll(Arrays.asList(head, body)) : html.getInheritings().info();
		assert html.getInheritings().size() == 2;
		assert html.getSubInheritings().containsAll(Arrays.asList(html, head, body, div));
		assert html.getSubInheritings().size() == 4;

		assert head.getInheritings().isEmpty();
		assert head.getSubInheritings().contains(head);
		assert head.getSubInheritings().size() == 1;

		assert body.getInheritings().contains(div);
		assert body.getInheritings().size() == 1;
		assert body.getSubInheritings().containsAll(Arrays.asList(body, div));
		assert body.getSubInheritings().size() == 2;

		assert div.getInheritings().isEmpty();
		assert div.getSubInheritings().contains(div);
		assert div.getSubInheritings().size() == 1;

	}

	public void test_MetaAttributeTest1() {
		CocClientEngine root = new CocClientEngine();
		Generic metaAttribute = root.getMetaAttribute();
		assert metaAttribute != null;
		assert root.getLevel() == 0;
		assert metaAttribute == root.setInstance(root.getValue(), root);
		assert metaAttribute.getLevel() == 0;
		assert metaAttribute.isMeta();
		assert metaAttribute.getMeta() == metaAttribute;
		assert metaAttribute.inheritsFrom(root) : metaAttribute.info();
		assert metaAttribute.isInstanceOf(root);
		assert metaAttribute.isInstanceOf(metaAttribute);
		assert !root.getInstances().contains(metaAttribute);
		assert !root.getSubInstances().contains(metaAttribute);
		assert metaAttribute.getBaseComponent().equals(root);
	}

	public void test_MetaRelationTest1() {
		CocClientEngine root = new CocClientEngine();
		Generic metaRelation = root.getMetaRelation();
		assert metaRelation != null;
		assert root.getLevel() == 0;
		assert metaRelation.getLevel() == 0;
		assert metaRelation.isMeta();
		assert metaRelation.getMeta() == metaRelation;
		assert metaRelation.inheritsFrom(root);
		assert metaRelation.isInstanceOf(root);
		assert metaRelation.isInstanceOf(metaRelation);
		assert !root.getInstances().contains(metaRelation);
		assert !root.getSubInstances().contains(metaRelation);
	}

	public void test_MetaRelationTest2() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic rootNode = tree.addInstance("rootNode");

		assert tree.equals(rootNode.getMeta()) : rootNode.detailedInfo();
		assert rootNode.getSupers().isEmpty();

		assert tree.getInstances().contains(rootNode);
		assert tree.getInstances().size() == 1;
		assert tree.getSubInstances().toList().equals(tree.getAsyncSubInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert tree.getSubInstances().contains(rootNode) : tree.getSubInstances().stream().collect(Collectors.toList());
		assert tree.getSubInstances().size() == 1;
		assert rootNode.getSupers().isEmpty();
	}

	public void test_TreeTest5() {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("tree");
		Generic rootNode = tree.addInstance("rootNode");
		Generic htmlNode = tree.addInstance(rootNode, "htmlNode");
		Generic bodyNode = tree.addInstance(htmlNode, "bodyNode");
		Generic divNode = tree.addInstance(bodyNode, "divNode");
		Generic formNode = tree.addInstance(divNode, "formNode");

		assert tree.getSubInstances().contains(rootNode);
		assert tree.getSubInstances().contains(bodyNode);
		assert tree.getSubInstances().contains(divNode);
		assert tree.getSubInstances().contains(formNode);
		assert tree.getSubInstances().size() == 5;
	}

	public void test105_remove_attribute_attribute_KO() throws InterruptedException, ExecutionException, TimeoutException {
		// given
		Generic engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power);

		assert vehicle.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert power.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert unit.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		vehicle.remove();
		assert !vehicle.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert !vehicle.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicle);
		assert !engine.getAsyncSubInstances().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(power);

		assert !power.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert !unit.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_getInstanceTest23() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic car = root.addInstance(vehicle, "Car");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic myBmw = car.addInstance("myBmw");
		Generic myAudi = car.addInstance("myAudi");
		Generic myBmwRed = vehicleColor.addInstance("", myBmw, red);
		vehicleColor.addInstance("", myAudi, red);
		Generic myBmwBlue = vehicleColor.addInstance("myBmwBlue", myBmw, blue);

		assert vehicleColor.getAsyncSubInstances(myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert vehicleColor.getAsyncSubInstances(myBmw).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).containsAll(Arrays.asList(myBmwRed, myBmwBlue));
	}

	public void test_getInheriting1() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		root.addInstance(car, "Vehicle");

		assert root.getAsyncInheriting("Vehicle").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == null;
		assert vehicle.getAsyncInheriting("Car").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == car;
	}

	public void test_getInheriting2() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		assert father.getAsyncInheriting("children1").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == children1;
	}

	public void test_getInheriting3() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic vehicleRed = vehicleColor.addInstance("", vehicle, red);
		Generic myVehicleRed = vehicleColor.addInstance("", myVehicle, red);
		vehicleColor.addInstance("", myVehicle, blue);

		assert vehicleRed.getAsyncInheriting("", myVehicle, red).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == myVehicleRed;
	}

	public void test_getInheritingTest4() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic vehicleRed = vehicleColor.addInstance("", vehicle, red);
		Generic myVehicleRed = vehicleColor.addInstance("", myVehicle, red);
		vehicleColor.addInstance("", myVehicle, blue);

		assert vehicleRed.getAsyncInheriting(red).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == myVehicleRed;
	}

	public void test_getInheritingTest5() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic vehicleRed = vehicleColor.addInstance("", vehicle, red);
		vehicleColor.addInstance("", myVehicle, red);
		Generic myVehicleRed2 = vehicleColor.addInstance(vehicleRed, "myVehicleRed2", myVehicle, red);
		vehicleColor.addInstance("", myVehicle, blue);

		assert vehicleRed.getAsyncInheritings("myVehicleRed2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert vehicleRed.getAsyncInheritings("myVehicleRed2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == myVehicleRed2;
	}

	public void test_getInheritingTest6() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic vehicleRed = vehicleColor.addInstance("", vehicle, red);
		vehicleColor.addInstance("", myVehicle, red);
		Generic myVehicleBlue = vehicleColor.addInstance(vehicleRed, "", myVehicle, blue);

		assert vehicleRed.getAsyncInheritings("").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert vehicleRed.getAsyncInheritings("", blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert vehicleRed.getAsyncInheritings("", blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == myVehicleBlue;
	}

	public void test_getInheritingTest7() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic vehicleRed = vehicleColor.addInstance("", vehicle, red);
		vehicleColor.addInstance("", myVehicle, red);
		Generic myVehicleBlue = vehicleColor.addInstance(vehicleRed, "", myVehicle, blue);

		assert vehicleRed.getAsyncInheritings(blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert vehicleRed.getAsyncInheritings(blue).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == myVehicleBlue;
	}

	public void test_getInheritingTest8() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic carVehicle = root.addInstance(car, "Vehicle");

		assert vehicle.getAsyncSubInheritings("Vehicle").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().containsAll(Arrays.asList(vehicle, carVehicle)) : root.getMetaAttribute().getSubInheritings("Vehicle").info();// .toList()
		assert vehicle.getAsyncSubInheritings("Car").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == car;
	}

	public void test_getInheritingTest9() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic sportCar = root.addInstance(car, "SportCar");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic carColor = car.addRelation("vehicleColor", color);
		Generic bikeColor = bike.addRelation("vehicleColor", color);
		Generic sportCarColor = sportCar.addRelation("vehicleColor", color);

		assert vehicleColor.getAsyncSubInheritings("vehicleColor", color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 4 : vehicleColor.getSubInheritings("vehicleColor", color).info();
		assert vehicleColor.getAsyncSubInheritings("vehicleColor", color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().containsAll(Arrays.asList(vehicleColor, carColor, bikeColor, sportCarColor));
	}

	public void test_getInheritingTest10() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic sportCar = root.addInstance(car, "SportCar");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);
		Generic carColor = car.addRelation("vehicleColor", color);
		bike.addRelation("vehicleColor", color);
		sportCar.addRelation("vehicleColor", color);

		assert vehicleColor.getAsyncInheritings(color, car).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : vehicleColor.getSubInheritings(color, car).info();
		assert vehicleColor.getAsyncInheritings(color, car).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(carColor);
	}

	public void test_getCompositeTest1() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("power");
		vehicle.addAttribute("option");

		assert vehicle.getAsyncComposite("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == power;
	}

	public void test_getCompositeTest2() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		vehicle.addAttribute("power");
		Generic carPower = car.addAttribute("power");
		vehicle.addAttribute("option");

		assert car.getAsyncComposites("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == carPower;
	}

	public void test_cacheTest1() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic powerVehicle = engine.addInstance("power", vehicle);
		assert vehicle.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(powerVehicle);
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic myVehicle123 = powerVehicle.addInstance("123", myVehicle);
		assert myVehicle.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myVehicle123);
	}

	public void test002_getSuperComponents() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic powerVehicle = engine.addInstance("power", vehicle);
		powerVehicle.enablePropertyConstraint();
		assert powerVehicle.isPropertyConstraintEnabled();
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic vehicle256 = powerVehicle.addInstance("256", vehicle);
		Generic myVehicle123 = powerVehicle.addInstance("123", myVehicle);
		assert !vehicle256.equals(myVehicle123);
		assert myVehicle123.inheritsFrom(vehicle256);
		assert myVehicle.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myVehicle123);
	}

	public void test_updateTest3() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("MyBmw");
		Generic myBmw233 = myBmw.addHolder(power, 233);

		assert myBmw.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myBmw233);
		assert myBmw.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert myBmw233.getValue().equals(233);

		Generic myBmw455 = myBmw233.updateValue(455);

		assert !myBmw233.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert myBmw.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myBmw455);
		assert myBmw.getAsyncComposites().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert myBmw455.getValue().equals(455);
	}

	// public void test_multiInheritance() throws InterruptedException, ExecutionException, TimeoutException {
	// CocClientEngine engine = new CocClientEngine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
	// Generic robot = engine.addInstance("Robot");
	// Generic robotSizable = engine.addInstance("Sizable", robot);
	// Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
	// // assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
	// Generic transformerSizable = engine.addInstance("Sizable", transformer);
	// // assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(transformerSizable);
	// assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
	// assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
	//
	// }
	//
	// public void test_multiInheritanceWithDiamond() throws InterruptedException, ExecutionException, TimeoutException {
	// CocClientEngine engine = new CocClientEngine();
	// Generic object = engine.addInstance("Object");
	// Generic objectSizable = engine.addInstance("Sizable", object);
	// Generic vehicle = engine.addInstance(Arrays.asList(object), "Vehicle");
	// assert vehicle.inheritsFrom(object);
	// Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
	// assert vehicleSizable.inheritsFrom(objectSizable);
	// Generic robot = engine.addInstance(Arrays.asList(object), "Robot");
	// assert robot.inheritsFrom(object);
	// Generic robotSizable = engine.addInstance("Sizable", robot);
	// assert robotSizable.inheritsFrom(objectSizable);
	// Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
	// assert transformer.inheritsFrom(vehicle);
	// assert transformer.inheritsFrom(robot);
	// // assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
	// Generic transformerSizable = engine.addInstance("Sizable", transformer);
	// // assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(transformerSizable);
	// assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
	// assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
	//
	// }
	//
	// public void test_ClassFinderTest5() throws InterruptedException, ExecutionException, TimeoutException {
	// Generic engine = new CocClientEngine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic robot = engine.addInstance("robot");
	// Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
	// Generic vehiclePower = engine.addInstance("Power", vehicle);
	// Generic robotPower = engine.addInstance("Power", robot);
	// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).containsAll(Arrays.asList(robotPower, vehiclePower)) : transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT,
	// Statics.SERVER_TIMEOUT_UNIT);
	// }
}
