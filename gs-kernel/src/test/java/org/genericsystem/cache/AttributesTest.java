package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class AttributesTest extends AbstractClassicTest {

	public void test1Attribut() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		assert vehicle.getLevel() == 1 : vehicle.getLevel();
		ClientGeneric power = engine.addInstance("Power", vehicle);
		assert power.getComponents().size() == 1;
		assert vehicle.equals(power.getComponents().get(0));
		assert power.isAlive();
	}

	public void test1AttributWith2LevelsInheritance1AttributOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		// assert vehicle.getAttributes(engine).size() == 1 : vehicle.getAttributes(engine);
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance1AttributOnFistChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);

		assert engine.getLevel() == 0;
		assert vehicle.getLevel() == 1;
		assert car.getLevel() == 1;
		assert power.getLevel() == 1;

		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 1;
		assert microcar.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnFirstChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 1;
		assert microcar.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnSecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		ClientGeneric power = engine.addInstance("Power", microcar);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 0;
		assert !car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 1;
		assert microcar.getAttributes(engine).contains(power);
	}

	/*
	 * public void testSimple1MetaAttribut() { Engine engine = new Engine(); Generic car = engine.addInstance("Car"); Generic power = engine.addInstance("Power", car); assert power.getCompositesStream().count() == 1; assert
	 * car.equals(power.getComposites()[0]); assert power.isAlive(); }
	 */
	public void test2Attributs() {

		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", vehicle);
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		assert power.isAlive();
		assert airconditioner.isAlive();
	}

	public void test2AttributsWith2LevelsInheritance2AttributsOnParent() {

		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith2LevelsInheritance2AttributsOnFistChild() {

		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", car);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith2LevelsInheritance1AttributOnParentAnd1AttributOnFistChild() {

		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", car);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance2AttributOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance2AttributFirstChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", car);
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance2AttributOnSecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		ClientGeneric power = engine.addInstance("Power", microcar);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", microcar);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 0;
		assert !car.getAttributes(engine).contains(power);
		assert !car.getAttributes(engine).contains(airconditioner);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnParent1AttributOnFirstChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", car);
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnParent1AttributOnSecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", microcar);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance1AttributFirstChild1AttributOnSecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);
		ClientGeneric microcar = engine.addInstance(car, "Microcar");
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", microcar);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		assert !car.getAttributes(engine).contains(airconditioner);
		// assert microcar.getAttributes(engine).size() == 2;
		assert microcar.getAttributes(engine).contains(power);
		assert microcar.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith2LevelsInheritance2ChildrenAt2ndLevel1AttributOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric caravan = engine.addInstance(vehicle, "Caravan");
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 1;
		assert caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance2ChildrenAt2ndLevel1AttributOnLevel1FirstChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric power = engine.addInstance("Power", car);
		ClientGeneric caravan = engine.addInstance(vehicle, "Caravan");
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 0;
		assert !caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance2ChildrenAt2ndLevel1AttributOnLevel1SecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric car = engine.addInstance(vehicle, "Car");
		ClientGeneric caravan = engine.addInstance(vehicle, "Caravan");
		ClientGeneric power = engine.addInstance("Power", caravan);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 0;
		assert !car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 1;
		assert caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel1AttributOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric power = engine.addInstance("Power", object);
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 1;
		assert object.getAttributes(engine).contains(power);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert robot.getAttributes(engine).size() == 1;
		assert robot.getAttributes(engine).contains(power);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel1AttributLevel1FistChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert robot.getAttributes(engine).size() == 0;
		assert !robot.getAttributes(engine).contains(power);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel1AttributLevel1SecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert robot.getAttributes(engine).size() == 0;
		assert !robot.getAttributes(engine).contains(power);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel1AttributLevel2Child1() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		ClientGeneric power = engine.addInstance("Power", transformer);
		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert robot.getAttributes(engine).size() == 0;
		assert !robot.getAttributes(engine).contains(power);
		// assert transformer.getAttributes(engine).size() == 1;
		assert transformer.getAttributes(engine).contains(power);
	}

	public void test2AttributsWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel2AttributsOnParent() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric power = engine.addInstance("Power", object);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", object);

		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 2 : object.getAttributes(engine);
		assert object.getAttributes(engine).contains(power);
		assert object.getAttributes(engine).contains(airconditioner);
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		// assert robot.getAttributes(engine).size() == 2;
		assert robot.getAttributes(engine).contains(power);
		assert robot.getAttributes(engine).contains(airconditioner);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(power);
		assert transformer.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel2AttributsLevel1FirstChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric power = engine.addInstance("Power", vehicle);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", vehicle);
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		assert !object.getAttributes(engine).contains(airconditioner);
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		// assert robot.getAttributes(engine).size() == 0;
		assert !robot.getAttributes(engine).contains(power);
		assert !robot.getAttributes(engine).contains(airconditioner);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(power);
		assert transformer.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel2AttributsLevel1SecondChild() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric power = engine.addInstance("Power", robot);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", robot);
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		assert !object.getAttributes(engine).contains(airconditioner);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert robot.getAttributes(engine).size() == 2;
		assert robot.getAttributes(engine).contains(power);
		assert robot.getAttributes(engine).contains(airconditioner);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(power);
		assert transformer.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel2AttributsLevel2() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric object = engine.addInstance("Object");
		ClientGeneric vehicle = engine.addInstance(object, "Vehicle");
		ClientGeneric robot = engine.addInstance(object, "Robot");
		ClientGeneric transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		ClientGeneric power = engine.addInstance("Power", transformer);
		ClientGeneric airconditioner = engine.addInstance("AirConditioner", transformer);
		// assert object.getAttributes(engine).size() == 0;
		assert !object.getAttributes(engine).contains(power);
		assert !object.getAttributes(engine).contains(airconditioner);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert robot.getAttributes(engine).size() == 0;
		assert !robot.getAttributes(engine).contains(power);
		assert !robot.getAttributes(engine).contains(airconditioner);
		// assert transformer.getAttributes(engine).size() == 2;
		assert transformer.getAttributes(engine).contains(power);
		assert transformer.getAttributes(engine).contains(airconditioner);
	}

}
