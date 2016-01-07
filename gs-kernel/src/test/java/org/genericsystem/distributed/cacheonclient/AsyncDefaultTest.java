package org.genericsystem.distributed.cacheonclient;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import javafx.collections.ObservableList;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.Snapshot;
import org.genericsystem.api.core.exceptions.AliveConstraintViolationException;
import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.genericsystem.common.Generic;
import org.genericsystem.common.HeavyCache;
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
		catchAndCheckCausePromise(root.getRoot().getMetaAttribute().getAsyncInstance(Collections.emptyList(), "power"), AmbiguousSelectionException.class);
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

	public void test_getInstanceTest24() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(Collections.emptyList(), "power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(Collections.emptyList(), "power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == vehiclePower;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(vehiclePower, "power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(vehiclePower, "power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).containsAll(Arrays.asList(bikePower, trunckPower));
	}

	public void test_getInstanceTest25() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic tree = root.addInstance("Tree");
		Generic father = tree.addInstance("father");
		Generic mother = tree.addInstance("mother");
		Generic children1 = tree.addInstance(Arrays.asList(father, mother), "children1");
		Generic children2 = tree.addInstance(Arrays.asList(father, mother), "children2");
		tree.addInstance(children1, "children2");

		assert tree.getAsyncSubInstances(Arrays.asList(mother), "children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1; // computeAsyncAndCheckOverridesAreReached unimplemented
		assert tree.getAsyncSubInstances("children2").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList().get(0) == children2;// .toList().get(0), before .first()
	}

	public void test_getInstanceTest26() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic trunck = root.addInstance(vehicle, "Trunck");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic carPower = car.addAttribute("carPower");
		Generic bikePower = bike.addAttribute(vehiclePower, "power");
		Generic trunckPower = trunck.addAttribute("power");

		assert !carPower.inheritsFrom(vehiclePower);
		assert trunckPower.inheritsFrom(vehiclePower);

		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(Collections.emptyList(), "power", vehicle).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(Collections.emptyList(), "power", vehicle).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == vehiclePower;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(vehiclePower, "power", bike).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert root.getRoot().getMetaAttribute().getAsyncSubInstances(vehiclePower, "power", bike).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).first() == bikePower;
	}

	public void test_updatableServiceTetst100_addSuper_Type() throws InterruptedException, ExecutionException, TimeoutException {
		// given
		Generic engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance("Car");

		// when
		Generic result = car.updateSupers(vehicle);
		assert !car.equals(result);
		assert result.isAlive();
		// then
		assert engine.isAlive();
		assert vehicle.isAlive();
		assert !car.isAlive();

		// assert engine.getAllInstances().count() == 2;

		Generic newVehicle = engine.getAsyncInstance("Vehicle").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert newVehicle == vehicle;
		assert newVehicle.getAsyncInheritings().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : newVehicle.getInheritings().stream().collect(Collectors.toList());
		assert engine.getAsyncInstance(newVehicle, "Car").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).getSupers().size() == 1;
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

	public void test_multiInheritance() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
		Generic robot = engine.addInstance("Robot");
		Generic robotSizable = engine.addInstance("Sizable", robot);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
		Generic transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(transformerSizable);
		assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
		assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);

	}

	public void test_multiInheritanceWithDiamond() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic object = engine.addInstance("Object");
		Generic objectSizable = engine.addInstance("Sizable", object);
		Generic vehicle = engine.addInstance(Arrays.asList(object), "Vehicle");
		assert vehicle.inheritsFrom(object);
		Generic vehicleSizable = engine.addInstance("Sizable", vehicle);
		assert vehicleSizable.inheritsFrom(objectSizable);
		Generic robot = engine.addInstance(Arrays.asList(object), "Robot");
		assert robot.inheritsFrom(object);
		Generic robotSizable = engine.addInstance("Sizable", robot);
		assert robotSizable.inheritsFrom(objectSizable);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		assert transformer.inheritsFrom(vehicle);
		assert transformer.inheritsFrom(robot);
		// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 2;
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
		Generic transformerSizable = engine.addInstance("Sizable", transformer);
		// assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(transformerSizable);
		assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(robotSizable);
		assert !transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleSizable);

	}

	public void test_ClassFinderTest5() throws InterruptedException, ExecutionException, TimeoutException {
		Generic engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic robot = engine.addInstance("robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic vehiclePower = engine.addInstance("Power", vehicle);
		Generic robotPower = engine.addInstance("Power", robot);
		Snapshot<Generic> snap = transformer.getAsyncAttributes(engine).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert snap.containsAll(Arrays.asList(robotPower, vehiclePower)) : transformer.getAttributes(engine);
	}

	public void test_attributesTest22() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new CocClientEngine();
		Generic object = root.addInstance("Object");
		Generic vehicle = root.addInstance(object, "Vehicle");
		Generic robot = root.addInstance(object, "Robot");
		Generic transformer = root.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic power = root.addInstance("Power", transformer);
		Generic airconditioner = root.addInstance("AirConditioner", transformer);
		assert !object.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(power);
		assert !object.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(airconditioner);
		assert !robot.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(power);
		assert !robot.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(airconditioner);
		assert !vehicle.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(power);
		assert !vehicle.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(airconditioner);
		assert transformer.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(power);
		assert transformer.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(airconditioner);
	}

	public void test_vertexTest10() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic sportCar = root.addInstance(car, "SportCar");
		Generic vehiclePower = root.addInstance("VehiclePower", vehicle);
		Generic carPower = root.addInstance(vehiclePower, "CarPower", car);
		Generic sportCarPower = root.addInstance(vehiclePower, "SportCarPower", sportCar);
		assert sportCar.getAsyncAttributes(root).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).containsAll(Arrays.asList(carPower, sportCarPower)) : car.getAttributes(root) + " " + sportCarPower.info();
	}

	public void test_relationTest9() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		Generic myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		assert carColorMat.getAsyncAttributes().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleColorIsCold);
		assert vehicleColor.getAsyncAttributes().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleColorIsCold);
	}

	public void test_AttributeTest1() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == vehicle.getAsyncAttribute("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_AttributeTest2() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic powerVehicle = vehicle.addAttribute("power");
		assert powerVehicle == vehicle.getAsyncAttribute("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	// public void test_AttributeTest3() throws InterruptedException, ExecutionException, TimeoutException {
	// BasicEngine root = new BasicEngine();
	// Generic vehicle = root.addInstance("Vehicle");
	// Generic myVehicle = vehicle.addInstance("myVehicle");
	// Generic powerVehicle = vehicle.addAttribute("power");
	// assert powerVehicle == myVehicle.getAsyncAttribute("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// }
	//
	// public void test_AttributeTest4() throws InterruptedException, ExecutionException, TimeoutException {
	// BasicEngine root = new BasicEngine();
	// Generic vehicle = root.addInstance("Vehicle");
	// Generic car = root.addInstance(vehicle, "Car");
	// Generic powerVehicle = vehicle.addAttribute("power");
	// assert powerVehicle == car.getAsyncAttribute("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// }
	//
	// public void test_AttributeTest5() throws InterruptedException, ExecutionException, TimeoutException {
	// BasicEngine root = new BasicEngine();
	// Generic vehicle = root.addInstance("Vehicle");
	// Generic car = root.addInstance(vehicle, "Car");
	// Generic myCar = car.addInstance("myCar");
	// Generic powerVehicle = vehicle.addAttribute("power");
	// assert powerVehicle == myCar.getAsyncAttribute("power").get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	// }

	public void test_removeOneCacheTest2_multipleHolders() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		HeavyCache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");

		myBmwRed.remove();
		cache.flush();
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		assert myBmw.getAsyncHolders(color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;

		Generic myBmwGreen = myBmw.addHolder(color, "green");

		catchAndCheckCause(() -> myBmwBlue.remove(), AliveConstraintViolationException.class);

		assert myBmw.getAsyncHolders(color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;
	}

	public void testRemoveOneCacheTest5_removeAndAddAndRemove() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		HeavyCache cache = engine.getCurrentCache();
		Generic car = engine.addInstance("Car");
		Generic color = car.addAttribute("Color");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmwRed = myBmw.addHolder(color, "red");
		cache.flush();
		cache.clear();
		Generic myBmwBlue = myBmw.addHolder(color, "blue");
		cache.clear();
		myBmwRed.remove();
		Generic myBmwRed2 = myBmw.addHolder(color, "red");
		cache.clear();
		assert myBmwRed.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert !myBmwRed2.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		myBmwRed.remove();
		assert myBmw.getAsyncHolders(color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;
	}

	public void test_holderTestHolderOverrideWithDifferentValue2ChainedAttributsWith3LevelsInheritance1AttributOnParentOverrideOnFirstChild() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power1 = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power1);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic power2 = engine.addInstance("Power", car);
		Generic microcar = engine.addInstance(Arrays.asList(car), "Microcar");

		// same value for power1 and power2
		int powerValue = 1;
		String unitValue1 = "Watt";
		String unitValue2 = "KWatt";

		Generic v1 = power1.addInstance(powerValue, vehicle);
		Generic v2 = power2.addInstance(Arrays.asList(v1), powerValue, car);

		Generic vUnit1 = unit.addInstance(unitValue1, power1);
		Generic vUnit2 = unit.addInstance(Arrays.asList(vUnit1), unitValue2, power2);

		assert !power1.equals(power2);
		assert v1.isInstanceOf(power1);
		assert v2.isInstanceOf(power1);

		assert !v1.isInstanceOf(power2);
		assert v2.isInstanceOf(power2);
		assert vUnit1.isInstanceOf(unit);
		assert vUnit2.isInstanceOf(unit);

		assert vehicle.getAsyncHolders(power1) != null;
		assert vehicle.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : vehicle.getHolders(power1);
		assert vehicle.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v1) : vehicle.getHolders(power1);

		assert power1.getAsyncHolders(unit) != null;
		assert power1.getAsyncHolders(unit).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : power1.getHolders(unit);
		assert power1.getAsyncHolders(unit).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vUnit1) : power1.getHolders(unit);

		assert power2.getAsyncHolders(unit) != null;
		assert power2.getAsyncHolders(unit).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : power2.getHolders(unit);
		assert power2.getAsyncHolders(unit).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vUnit2) : power2.getHolders(unit);

		assert microcar.getAsyncHolders(power1) != null;
		assert microcar.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : microcar.getHolders(power1);
		assert microcar.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v2) : microcar.getHolders(power1);

		assert car.getAsyncHolders(power1) != null;
		assert car.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : car.getHolders(power1);
		assert car.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v2) : car.getHolders(power1);

		assert power1.getInstances() != null;
		assert power1.getInstances().size() == 1;
		assert power1.getInstances().contains(v1);

		assert power1.getInstance(powerValue, vehicle) != null;
		assert power1.getInstance(powerValue, vehicle).equals(v1);
		assert power1.getInstance(powerValue, car) == null;

		assert power2.getInstance(powerValue, car) != null;
		assert power2.getInstance(powerValue, car).equals(v2);
		assert power2.getInstance(powerValue, microcar) == null;

		assert unit.getInstances() != null;
		assert unit.getInstances().size() == 2;
		assert unit.getInstances().contains(vUnit1);
		assert unit.getInstances().contains(vUnit2);

		assert unit.getInstance(unitValue1, power1) != null;
		assert unit.getInstance(unitValue1, power1).equals(vUnit1);

		assert unit.getInstance(unitValue2, power2) != null;
		assert unit.getInstance(unitValue2, power2).equals(vUnit2);

		assert microcar.getAsyncHolders(power2) != null;
		assert microcar.getAsyncHolders(power2).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : microcar.getHolders(power2);
		assert microcar.getAsyncHolders(power2).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v2) : microcar.getHolders(power2);

		assert microcar.getAsyncHolders(power1) != null : microcar.getHolders(power1);
		assert microcar.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1 : microcar.getHolders(power1);
		assert microcar.getAsyncHolders(power1).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(v2) : microcar.getHolders(power1);

		assert microcar.getAsyncHolders(unit) != null;
		assert microcar.getAsyncHolders(unit).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;

		assert power1.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert power2.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert v1.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert v2.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_nonHeritableTest6() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic defaultPower = car.addHolder(power, 233);
		Generic myCar = car.addInstance("myCar");
		power.disableInheritance();
		assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == null;
		power.enableInheritance();
		assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).equals(defaultPower);
	}

	public void test_nonHeritableTest7() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");
		Generic car = root.addInstance(vehicle, "Car");
		Generic defaultPower = vehicle.addHolder(power, 233);
		assert defaultPower.equals(car.getHolder(power));
		assert car.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) != null;
		power.disableInheritance();
		assert car.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == null;
		Generic defaultCarPower = car.addHolder(power, defaultPower, 256);
		Generic myCar = car.addInstance("myBmw");
		assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == null;
		power.enableInheritance();
		assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) != null;
		assert defaultCarPower.equals(myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)) : myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_nonHeritableTest16() throws InterruptedException, ExecutionException, TimeoutException {
		final CocClientEngine root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		Generic myCar256 = myCar.setHolder(power, 256);
		assert myCar256.equals(myCar.getAsyncHolder(power, 256).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public void test_nonHeritableTest17() throws InterruptedException, ExecutionException, TimeoutException {
		final CocClientEngine engine = new CocClientEngine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic carDefaultPower = car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		myCar.setHolder(power, 256);
		assert carDefaultPower.equals(car.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public void test_ajustMetaTest17() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);
		Generic carColor = car.addRelation(vehicleColor, "CarColor", color);
		assert carColor.inheritsFrom(vehicleColor);

		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		assert myBmw.getAsyncRelations(ApiStatics.BASE_POSITION).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(carColor);
		assert !myBmw.getAsyncRelations(ApiStatics.TARGET_POSITION).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(carColor);
		Generic myBmwRed = myBmw.addLink(vehicleColor, "myBmwRed", red);
		assert carColor.equals(myBmwRed.getMeta());
	}

	public void test_testLink10() throws InterruptedException, ExecutionException, TimeoutException {
		final CocClientEngine root = new CocClientEngine();
		Generic humain = root.addInstance("Human");
		Generic hierarchy = humain.addRelation("Hierarchy", humain);
		Generic nicolas = humain.addInstance("nicolas");
		humain.addInstance("michael");
		assert hierarchy.equals(nicolas.getAsyncRelation(humain).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT)) : nicolas.getAsyncRelations().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).info();
		assert hierarchy.equals(nicolas.getAsyncRelation(humain).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
		assert null == nicolas.getAsyncRelation(humain, humain).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_attributesTest25() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic color = root.addInstance("Color");
		Generic ultraColor = root.addInstance(color, "UltraColor");
		vehicle.addRelation("colorVehicle", color);
		Generic ultraColorVehicle = vehicle.addRelation("colorVehicle", ultraColor);
		assert null == myVehicle.getAsyncRelation("colorVehicle", vehicle, color).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert ultraColorVehicle == myVehicle.getAsyncRelation("colorVehicle", ultraColor).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test012() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");
		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");
		Generic carColor = car.addRelation("CarColor", color);
		carColor.enableSingularConstraint(ApiStatics.BASE_POSITION);
		Generic carRed = car.addLink(carColor, "CarRed", red);
		assert carRed.isSuperOf(carColor, Collections.emptyList(), "myBmwYellow", Arrays.asList(myBmw, yellow));
		Generic myBmwYellow = myBmw.addLink(carColor, "myBmwYellow", yellow);
		assert myBmw.getAsyncLinks(carColor).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myBmwYellow);
		assert myBmw.getAsyncLinks(carColor).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
		assert yellow.getAsyncLinks(carColor).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myBmwYellow);
		assert yellow.getAsyncLinks(carColor).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;
	}

	public void test_componentsOrderTest12() throws InterruptedException, ExecutionException, TimeoutException {
		final CocClientEngine engine = new CocClientEngine();
		Generic car = engine.addInstance("Car");
		final Generic largerThan = engine.addInstance("largerThan", car, car);
		final Generic myBmw = car.addInstance("myBmw");
		final Generic myAudi = car.addInstance("myAudi");
		final Generic myMercedes = car.addInstance("myMercedes");
		final Generic myPorsche = car.addInstance("myPorsche");
		largerThan.setInstance("myBmwLargerThanMyAudi", myBmw, myAudi);
		largerThan.setInstance("myMercedesLargerThanMyBmw", myMercedes, myBmw);
		largerThan.setInstance("myBmwLargerThanMymyPorsche", myBmw, myPorsche);
		List<Generic> smallerThanMyMbw = myBmw.getAsyncLinks(largerThan, ApiStatics.BASE_POSITION).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).stream().map(Generic::getTargetComponent).collect(Collectors.toList());
		assert smallerThanMyMbw.size() == 2;
		assert smallerThanMyMbw.contains(myAudi);
		assert smallerThanMyMbw.contains(myPorsche);
	}

	public void test_linkTest11() throws InterruptedException, ExecutionException, TimeoutException {
		final CocClientEngine root = new CocClientEngine();
		Generic humain = root.addInstance("Human");
		Generic hierarchy = humain.addRelation("Hierarchy", humain);
		Generic nicolas = humain.addInstance("nicolas");
		Generic michael = humain.addInstance("michael");
		Generic michaelMichael = michael.addLink(hierarchy, "michaelMichael", michael);
		michael.addLink(hierarchy, "michaelNicolas", nicolas);

		assert michael.getAsyncLink(hierarchy, michael).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == michaelMichael;
	}

	public void test_linkTest2() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic outsideColor = root.addInstance(color, "OutsideColor");

		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		Generic outsideRed = outsideColor.addInstance("OutsideRed");

		Generic carColor = car.addRelation("carColor", color);
		Generic carRed = car.addLink(carColor, "carRed", red);

		Generic carOutsideColor = car.addRelation(carColor, "CarOutsideColor", outsideColor);
		Generic carOutsideRed = car.addLink(carOutsideColor, "carOutsideRed", outsideRed);

		myBmw.addLink(carOutsideColor, carOutsideRed, "myBmwOutsideRed", outsideRed);

		carRed.conserveRemove();
		Generic myBmwRed = myBmw.getAsyncLink(carOutsideColor, "myBmwOutsideRed", outsideRed).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert myBmwRed != null;
		assert myBmwRed.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert myBmwRed.getSupers().size() == 1;
		assert myBmwRed.getSupers().get(0).equals(carOutsideColor, Collections.emptyList(), "carOutsideRed", Arrays.asList(car, outsideRed)) : myBmwRed.getSupers().get(0).info();
		assert !carRed.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_removeTest2() throws InterruptedException, ExecutionException, TimeoutException {
		Generic root = new CocClientEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic outsideColor = root.addInstance(color, "OutsideColor");

		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		Generic outsideRed = outsideColor.addInstance("OutsideRed");

		Generic carColor = car.addRelation("carColor", color);
		Generic carRed = car.addLink(carColor, "carRed", red);

		Generic carOutsideColor = car.addRelation(carColor, "CarOutsideColor", outsideColor);
		Generic carOutsideRed = car.addLink(carOutsideColor, "carOutsideRed", outsideRed);

		myBmw.addLink(carOutsideColor, carOutsideRed, "myBmwOutsideRed", outsideRed);

		carRed.conserveRemove();
		Generic myBmwRed = myBmw.getAsyncLink(carOutsideColor, "myBmwOutsideRed", outsideRed).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert myBmwRed != null;
		assert myBmwRed.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert myBmwRed.getSupers().size() == 1;
		assert myBmwRed.getSupers().get(0).equals(carOutsideColor, Collections.emptyList(), "carOutsideRed", Arrays.asList(car, outsideRed)) : myBmwRed.getSupers().get(0).info();
		assert !carRed.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_relationTest10() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
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
		Generic myVehicle125 = myVehicle.addHolder(vehiclePower, "125");

		assert myVehicle.getAsyncHolders(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(myVehicle125) : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle125.getValue().equals("125");
		assert myVehicle.getAsyncValues(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains("125") : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle.getAsyncHolders(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 1;

		assert myCar.getAsyncHolders(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;

	}

	// public void test_annotationTest8_SubRelation() {
	// CocClientEngine engine = new CocClientEngine(Car.class, Human.class, HumanPossessVehicle.class, HumanPossessCar.class);
	// engine.find(Car.class);
	// Generic human = engine.find(Human.class);
	// Generic possessVehicle = engine.find(HumanPossessVehicle.class);
	// Generic possessCar = engine.find(HumanPossessCar.class);
	// assert possessCar.inheritsFrom(possessVehicle);
	// assert human.getAttributes().contains(possessCar) : human.getAttributes();
	//
	// }

	// public void test_nonHeritableTest6() throws InterruptedException, ExecutionException, TimeoutException {
	// CocClientEngine root = new CocClientEngine();
	// Generic car = root.addInstance("Car");
	// Generic power = car.addAttribute("Power");
	// Generic defaultPower = car.addHolder(power, 233);
	// Generic myCar = car.addInstance("myCar");
	// power.disableInheritance();
	// assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT) == null;
	// power.enableInheritance();
	// assert myCar.getAsyncHolder(power).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).equals(defaultPower);
	// }

	// // // //

	// // // //

	// // // //

	// // // //
	public void test_getCompositeTest2_Obs() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		ObservableList<Generic> composites = car.getObservableComposites();
		vehicle.addAttribute("power");
		Generic carPower = car.addAttribute("power");
		vehicle.addAttribute("option");

		Thread.sleep(500);
		assert composites.size() == 1;
		assert composites.get(0) == carPower;

		Generic carOption = car.addAttribute("option");

		Thread.sleep(500);
		assert composites.size() == 2;
		assert composites.get(1) == carOption;
	}

	public void test_relationTest9_Obs() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		Generic myCarRedMat = myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		Generic myVehicleRed = myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic vehicleColorIsCold = vehicleColor.addAttribute("vehicleColorIsCold");

		ObservableList<Generic> attributes = carColorMat.getObservableAttributes();

		Thread.sleep(1000);
		System.out.println(attributes.size());
		System.out.println("obs  " + attributes);
		System.out.println("sync " + carColorMat.getAttributes().toList());
		System.out.println("async" + carColorMat.getAsyncAttributes().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).toList());
		assert attributes.contains(vehicleColorIsCold);
		assert vehicleColor.getAsyncAttributes().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains(vehicleColorIsCold);
	}

	public void test_holderTestHolderOverrideWithDifferentValue2ChainedAttributsWith3LevelsInheritance1AttributOnParentOverrideOnFirstChild_Obs() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power1 = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power1);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic power2 = engine.addInstance("Power", car);

		// same value for power1 and power2
		int powerValue = 1;
		String unitValue1 = "Watt";
		String unitValue2 = "KWatt";

		Generic v1 = power1.addInstance(powerValue, vehicle);
		Generic v2 = power2.addInstance(Arrays.asList(v1), powerValue, car);

		Generic vUnit1 = unit.addInstance(unitValue1, power1);
		Generic vUnit2 = unit.addInstance(Arrays.asList(vUnit1), unitValue2, power2);

		ObservableList<Generic> vehicleHolders = vehicle.getObservableHolders(power1);
		ObservableList<Generic> power1Holders = power1.getObservableHolders(unit);
		ObservableList<Generic> power2Holders = power2.getObservableHolders(unit);

		Thread.sleep(1000);
		vehicleHolders.size();
		power1Holders.size();
		power2Holders.size();

		assert !power1.equals(power2);
		assert v1.isInstanceOf(power1);
		assert v2.isInstanceOf(power1);

		assert !v1.isInstanceOf(power2);
		assert v2.isInstanceOf(power2);
		assert vUnit1.isInstanceOf(unit);
		assert vUnit2.isInstanceOf(unit);

		assert vehicleHolders != null;
		assert vehicleHolders.size() == 1 : vehicle.getHolders(power1);
		assert vehicleHolders.contains(v1) : vehicle.getHolders(power1);

		assert power1Holders != null;
		assert power1Holders.size() == 1 : power1.getHolders(unit);
		assert power1Holders.contains(vUnit1) : power1.getHolders(unit);

		assert power2Holders != null;
		assert power2Holders.size() == 1 : power2.getHolders(unit);
		assert power2Holders.contains(vUnit2) : power2.getHolders(unit);

		// ...

		assert power1.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert power2.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert v1.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
		assert v2.isAsyncAlive().get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT);
	}

	public void test_asyncSupersComputer() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("vehicle");
		Generic vehiclePower = vehicle.addAttribute("power");
		Generic car = root.addInstance(vehicle, "car");

		assert root.getCurrentCache().computeAndCheckOverridesAreReached(root.getRoot().getMetaAttribute(), new ArrayList<>(), "power", Arrays.asList(car))
				.equals(root.getCurrentCache().computeAsyncAndCheckOverridesAreReached(root.getRoot().getMetaAttribute(), new ArrayList<>(), "power", Arrays.asList(car)).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public void test_asyncSupersComputer2() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("vehicle");
		vehicle.addAttribute("power");
		Generic vehicle2 = root.addInstance(vehicle, "vehicle2");
		Generic vehicle3 = root.addInstance(vehicle2, "vehicle3");
		Generic vehicle4 = root.addInstance(vehicle3, "vehicle4");
		Generic car = root.addInstance(vehicle4, "car");

		assert root.getCurrentCache().computeAndCheckOverridesAreReached(root.getRoot().getMetaAttribute(), new ArrayList<>(), "power", Arrays.asList(car))
				.equals(root.getCurrentCache().computeAsyncAndCheckOverridesAreReached(root.getRoot().getMetaAttribute(), new ArrayList<>(), "power", Arrays.asList(car)).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public void test_asyncSupersComputer3() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("vehicle");
		Generic vehicle2 = root.addInstance(vehicle, "vehicle2");
		Generic vehicle3 = root.addInstance(vehicle2, "vehicle3");
		Generic vehicle4 = root.addInstance(vehicle3, "vehicle4");

		Generic color = root.addInstance("color");
		Generic color2 = root.addInstance(color, "color2");
		Generic color3 = root.addInstance(color2, "color3");

		vehicle.addRelation("vehicleColor", color);

		assert root.getCurrentCache().computeAndCheckOverridesAreReached(root.getRoot().getMetaRelation(), new ArrayList<>(), "vehicleColor", Arrays.asList(vehicle4, color3))
				.equals(root.getCurrentCache().computeAsyncAndCheckOverridesAreReached(root.getRoot().getMetaRelation(), new ArrayList<>(), "vehicleColor", Arrays.asList(vehicle4, color3)).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}

	public void test_asyncSupersComputer4() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine root = new CocClientEngine();
		Generic vehicle = root.addInstance("vehicle");
		Generic standard = vehicle.addInstance("standard");
		Generic myCar = vehicle.addInstance(standard, "myCar");

		Generic vehiclePower = vehicle.addAttribute("power");
		Generic v235 = standard.addHolder(vehiclePower, "235");
		vehiclePower.enableSingularConstraint(ApiStatics.BASE_POSITION);

		assert root.getCurrentCache().computeAndCheckOverridesAreReached(vehiclePower, new ArrayList<>(), "238", Arrays.asList(myCar))
				.equals(root.getCurrentCache().computeAsyncAndCheckOverridesAreReached(vehiclePower, new ArrayList<>(), "238", Arrays.asList(myCar)).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT));
	}
}
