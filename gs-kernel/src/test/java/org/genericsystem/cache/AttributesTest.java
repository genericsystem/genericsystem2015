package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class AttributesTest extends AbstractTest {

	public void test1Attribut() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		assert vehicle.getLevel() == 1 : vehicle.getLevel();
		Generic power = engine.addInstance("Power", vehicle);
		assert power.getComponents().size() == 1;
		assert vehicle.equals(power.getComponents().get(0));
		assert power.isAlive();
	}

	public void test1AttributWith2LevelsInheritance1AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		// assert vehicle.getAttributes(engine).size() == 1 : vehicle.getAttributes(engine);
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance1AttributOnFistChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);

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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 1;
		assert microcar.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnFirstChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic microcar = engine.addInstance(car, "Microcar");
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert microcar.getAttributes(engine).size() == 1;
		assert microcar.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance1AttributOnSecondChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");
		Generic power = engine.addInstance("Power", microcar);
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

		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic airconditioner = engine.addInstance("AirConditioner", vehicle);
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		assert power.isAlive();
		assert airconditioner.isAlive();
	}

	public void test2AttributsWith2LevelsInheritance2AttributsOnParent() {

		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic airconditioner = engine.addInstance("AirConditioner", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		// assert vehicle.getAttributes(engine).size() == 2;
		assert vehicle.getAttributes(engine).contains(power);
		assert vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith2LevelsInheritance2AttributsOnFistChild() {

		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic airconditioner = engine.addInstance("AirConditioner", car);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test2AttributsWith2LevelsInheritance1AttributOnParentAnd1AttributOnFistChild() {

		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic airconditioner = engine.addInstance("AirConditioner", car);
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		assert !vehicle.getAttributes(engine).contains(airconditioner);
		// assert car.getAttributes(engine).size() == 2;
		assert car.getAttributes(engine).contains(power);
		assert car.getAttributes(engine).contains(airconditioner);
	}

	public void test1AttributWith3LevelsInheritance2AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic airconditioner = engine.addInstance("AirConditioner", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic airconditioner = engine.addInstance("AirConditioner", car);
		Generic microcar = engine.addInstance(car, "Microcar");
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");
		Generic power = engine.addInstance("Power", microcar);
		Generic airconditioner = engine.addInstance("AirConditioner", microcar);
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic airconditioner = engine.addInstance("AirConditioner", car);
		Generic microcar = engine.addInstance(car, "Microcar");
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic microcar = engine.addInstance(car, "Microcar");
		Generic airconditioner = engine.addInstance("AirConditioner", microcar);
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic microcar = engine.addInstance(car, "Microcar");
		Generic airconditioner = engine.addInstance("AirConditioner", microcar);
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
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(vehicle, "Car");
		Generic caravan = engine.addInstance(vehicle, "Caravan");
		// assert vehicle.getAttributes(engine).size() == 1;
		assert vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 1;
		assert caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance2ChildrenAt2ndLevel1AttributOnLevel1FirstChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = engine.addInstance("Power", car);
		Generic caravan = engine.addInstance(vehicle, "Caravan");
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 1;
		assert car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 0;
		assert !caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith2LevelsInheritance2ChildrenAt2ndLevel1AttributOnLevel1SecondChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic caravan = engine.addInstance(vehicle, "Caravan");
		Generic power = engine.addInstance("Power", caravan);
		// assert vehicle.getAttributes(engine).size() == 0;
		assert !vehicle.getAttributes(engine).contains(power);
		// assert car.getAttributes(engine).size() == 0;
		assert !car.getAttributes(engine).contains(power);
		// assert caravan.getAttributes(engine).size() == 1;
		assert caravan.getAttributes(engine).contains(power);
	}

	public void test1AttributWith3LevelsInheritance2ChildrenAt2ndLevel1ChildAtThirdLevel1AttributOnParent() {
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic power = engine.addInstance("Power", object);
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic power = engine.addInstance("Power", vehicle);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic power = engine.addInstance("Power", transformer);
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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic power = engine.addInstance("Power", object);
		Generic airconditioner = engine.addInstance("AirConditioner", object);

		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic airconditioner = engine.addInstance("AirConditioner", vehicle);
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic power = engine.addInstance("Power", robot);
		Generic airconditioner = engine.addInstance("AirConditioner", robot);
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");

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
		Engine engine = new Engine();
		Generic object = engine.addInstance("Object");
		Generic vehicle = engine.addInstance(object, "Vehicle");
		Generic robot = engine.addInstance(object, "Robot");
		Generic transformer = engine.addInstance(Arrays.asList(vehicle, robot), "Transformer");
		Generic power = engine.addInstance("Power", transformer);
		Generic airconditioner = engine.addInstance("AirConditioner", transformer);
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
