package org.genericsystem.cache;

import java.util.Arrays;

import org.testng.annotations.Test;

@Test
public class HolderTest extends AbstractTest {

	public void testHolder1Attribut() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, vehicle);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, vehicle) != null;
		assert power.getInstance(powerValue, vehicle).equals(v1);

		assert power.isAlive();
		assert v1.isAlive();
	}

	public void testHolder1AttributWith2LevelsInheritance1AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, vehicle);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert car.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, vehicle).equals(v1);
		assert power.isAlive();
		assert v1.isAlive();
	}

	public void testHolderOverrideWithDifferentValue1AttributWith2LevelsInheritance1AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		int powerValue1 = 1;
		int powerValue2 = 2;
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, car);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert car.getHolders(power).contains(v2) : vehicle.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 2;
		assert power.getInstances().contains(v1);
		assert power.getInstances().contains(v2);
		assert power.getInstance(powerValue1, vehicle).equals(v1);
		assert power.getInstance(powerValue2, car).equals(v2) : power.getInstance(powerValue2, car);
		assert power.isAlive();

		assert v1.isAlive();
		assert v2.isAlive();
	}

	public void testHolderOverrideWithSameValue1AttributWith2LevelsInheritance1AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		int powerValue1 = 1;
		int powerValue2 = 1;
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, car);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v2) : car.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 2;
		assert power.getInstances().contains(v1);
		assert power.getInstances().contains(v2);
		assert power.getInstance(powerValue1, vehicle).equals(v1);
		assert power.getInstance(powerValue2, car).equals(v2) : power.getInstance(powerValue2, car);
		assert power.isAlive();

		assert v1.isAlive();
		assert v2.isAlive();
	}

	public void testHolder1AttributWith2LevelsInheritance1AttributOnFirstChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic power = engine.addInstance("Power", car);

		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, car);

		assert v1.isInstanceOf(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v1) : car.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, car).equals(v1);
	}

	public void testHolder1AttributWith2LevelsInheritance2children1AttributOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic bike = engine.addInstance(Arrays.asList(vehicle), "bike");
		Generic power = engine.addInstance("Power", vehicle);
		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, vehicle);

		assert v1.isInstanceOf(power);

		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v1) : car.getHolders(power);

		assert bike.getHolders(power) != null;
		assert bike.getHolders(power).size() == 1 : bike.getHolders(power);
		assert bike.getHolders(power).contains(v1) : bike.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, vehicle).equals(v1);
	}

	public void testHolderOverrideWithDifferentValue1AttributWith3LevelsInheritance1AttributOnParentOverrideOnFirstChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic microcar = engine.addInstance(Arrays.asList(car), "Microcar");

		int powerValue1 = 1;
		int powerValue2 = 1;
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, car);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v2) : car.getHolders(power);

		assert microcar.getHolders(power) != null;
		assert microcar.getHolders(power).size() == 1 : microcar.getHolders(power);
		assert microcar.getHolders(power).contains(v2) : microcar.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 2;
		assert power.getInstances().contains(v1);
		assert power.getInstances().contains(v2);
		assert power.getInstance(powerValue1, vehicle) != null;
		assert power.getInstance(powerValue1, vehicle).equals(v1);
		assert power.getInstance(powerValue2, car) != null;
		assert power.getInstance(powerValue2, car).equals(v2);
		assert power.getInstance(powerValue1, microcar) == null;

		assert power.isAlive();
		assert v1.isAlive();
		assert v2.isAlive();
	}

	public void testHolderOverrideWithDifferentValue1AttributWith3LevelsInheritance1AttributOnParentOverrideOnSecondChild() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");
		Generic microcar = engine.addInstance(Arrays.asList(car), "Microcar");

		int powerValue1 = 1;
		int powerValue2 = 1;
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, microcar);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v1) : car.getHolders(power);

		assert microcar.getHolders(power) != null;
		assert microcar.getHolders(power).size() == 1 : microcar.getHolders(power);
		assert microcar.getHolders(power).contains(v2) : microcar.getHolders(power);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 2;
		assert power.getInstances().contains(v1);
		assert power.getInstances().contains(v2);
		assert power.getInstance(powerValue1, vehicle) != null;
		assert power.getInstance(powerValue1, vehicle).equals(v1);
		assert power.getInstance(powerValue1, car) == null;
		assert power.getInstance(powerValue2, microcar) != null;
		assert power.getInstance(powerValue2, microcar).equals(v2);

		assert power.isAlive();
		assert v1.isAlive();
		assert v2.isAlive();
	}

	public void test2ChainedAttributs() {

		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power);
		int powerValue = 1;
		String unitValue = "Watt";

		Generic v1 = power.addInstance(powerValue, vehicle);
		Generic vUnit = unit.addInstance(unitValue, power);

		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert power.getHolders(unit) != null;
		assert power.getHolders(unit).size() == 1 : power.getHolders(unit);
		assert power.getHolders(unit).contains(vUnit) : power.getHolders(unit);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, vehicle) != null;
		assert power.getInstance(powerValue, vehicle).equals(v1);

		assert unit.getInstances() != null;
		assert unit.getInstances().size() == 1;
		assert unit.getInstances().contains(vUnit);
		assert unit.getInstance(unitValue, power) != null;
		assert unit.getInstance(unitValue, power).equals(vUnit);

		assert power.isAlive();
		assert v1.isAlive();

		assert unit.isAlive();
		assert vUnit.isAlive();
	}

	public void testHolderOverrideWithDifferentValue2ChainedAttributWith2LevelsInheritance2AttributsOnParent() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic power = engine.addInstance("Power", vehicle);
		Generic unit = engine.addInstance("Unit", power);

		Generic car = engine.addInstance(Arrays.asList(vehicle), "Car");

		int powerValue1 = 1;
		int powerValue2 = 2;
		String unitValue = "Watt";
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, car);

		Generic vUnit = unit.addInstance(unitValue, power);

		assert v1.isInstanceOf(power);
		assert v2.isInstanceOf(power);

		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);

		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert car.getHolders(power).contains(v2) : vehicle.getHolders(power);

		assert power.getHolders(unit) != null;
		assert power.getHolders(unit).size() == 1 : power.getHolders(unit);
		assert power.getHolders(unit).contains(vUnit) : power.getHolders(unit);

		assert power.getInstances() != null;
		assert power.getInstances().size() == 2;
		assert power.getInstances().contains(v1);
		assert power.getInstances().contains(v2);
		assert power.getInstance(powerValue1, vehicle).equals(v1);
		assert power.getInstance(powerValue2, car).equals(v2) : power.getInstance(powerValue2, car);
		assert power.isAlive();

		assert unit.getInstances() != null;
		assert unit.getInstances().size() == 1;
		assert unit.getInstances().contains(vUnit);
		assert unit.getInstance(unitValue, power) != null;
		assert unit.getInstance(unitValue, power).equals(vUnit);

		assert v1.isAlive();
		assert v2.isAlive();
		assert unit.isAlive();
		assert vUnit.isAlive();
	}

	public void testHolderOverrideWithDifferentValue2ChainedAttributsWith3LevelsInheritance1AttributOnParentOverrideOnFirstChild() {
		Engine engine = new Engine();
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

		assert vehicle.getHolders(power1) != null;
		assert vehicle.getHolders(power1).size() == 1 : vehicle.getHolders(power1);
		assert vehicle.getHolders(power1).contains(v1) : vehicle.getHolders(power1);

		assert power1.getHolders(unit) != null;
		assert power1.getHolders(unit).size() == 1 : power1.getHolders(unit);
		assert power1.getHolders(unit).contains(vUnit1) : power1.getHolders(unit);

		assert power2.getHolders(unit) != null;
		assert power2.getHolders(unit).size() == 1 : power2.getHolders(unit);
		assert power2.getHolders(unit).contains(vUnit2) : power2.getHolders(unit);

		assert microcar.getHolders(power1) != null;
		assert microcar.getHolders(power1).size() == 1 : microcar.getHolders(power1);
		assert microcar.getHolders(power1).contains(v2) : microcar.getHolders(power1);

		assert car.getHolders(power1) != null;
		assert car.getHolders(power1).size() == 1 : car.getHolders(power1);
		assert car.getHolders(power1).contains(v2) : car.getHolders(power1);

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

		assert microcar.getHolders(power2) != null;
		assert microcar.getHolders(power2).size() == 1 : microcar.getHolders(power2);
		assert microcar.getHolders(power2).contains(v2) : microcar.getHolders(power2);

		assert microcar.getHolders(power1) != null : microcar.getHolders(power1);
		assert microcar.getHolders(power1).size() == 1 : microcar.getHolders(power1);
		assert microcar.getHolders(power1).contains(v2) : microcar.getHolders(power1);

		assert microcar.getHolders(unit) != null;
		assert microcar.getHolders(unit).size() == 0;

		assert power1.isAlive();
		assert power2.isAlive();
		assert v1.isAlive();
		assert v2.isAlive();
	}
}
