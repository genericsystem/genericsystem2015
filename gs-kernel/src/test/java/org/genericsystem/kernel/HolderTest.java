package org.genericsystem.kernel;

import java.util.Arrays;

import org.genericsystem.api.core.exceptions.AmbiguousSelectionException;
import org.testng.annotations.Test;

@Test
public class HolderTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
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
		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, vehicle).equals(v1);
		assert power.isAlive();
		assert v1.isAlive();
	}

	public void test002() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		int powerValue1 = 1;
		int powerValue2 = 2;
		Generic v1 = power.addInstance(powerValue1, vehicle);
		Generic v2 = power.addInstance(Arrays.asList(v1), powerValue2, car);
		assert v1.isInstanceOf(power);
		assert vehicle.getHolders(power) != null;
		assert vehicle.getHolders(power).size() == 1 : vehicle.getHolders(power);
		assert vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);
		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : vehicle.getHolders(power).info();
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

	public void test003() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
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

	public void test004() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic power = root.addInstance("Power", car);
		int powerValue = 1;
		Generic v1 = power.addInstance(powerValue, car);
		assert v1.isInstanceOf(power);
		assert !(vehicle.getHolders(power).size() == 1) : vehicle.getHolders(power);
		assert !vehicle.getHolders(power).contains(v1) : vehicle.getHolders(power);
		assert car.getHolders(power) != null;
		assert car.getHolders(power).size() == 1 : car.getHolders(power);
		assert car.getHolders(power).contains(v1) : car.getHolders(power);
		assert power.getInstances() != null;
		assert power.getInstances().size() == 1;
		assert power.getInstances().contains(v1);
		assert power.getInstance(powerValue, car).equals(v1);
	}

	public void test005() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic bike = root.addInstance(Arrays.asList(vehicle), "bike");
		Generic power = root.addInstance("Power", vehicle);
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

	public void test006() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic microcar = root.addInstance(Arrays.asList(car), "Microcar");
		Generic v1 = power.addInstance(233, vehicle);
		Generic v2 = power.addInstance(v1, 233, car);
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
		assert power.getInstance(233, vehicle) != null;
		assert power.getInstance(233, vehicle).equals(v1);
		assert power.getInstance(233, car) != null;
		assert power.getInstance(233, car).equals(v2);
		assert power.getInstance(233, microcar) == null;
		assert power.isAlive();
		assert v1.isAlive();
		assert v2.isAlive();
	}

	public void test007() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic microcar = root.addInstance(Arrays.asList(car), "Microcar");
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

	public void test008() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic unit = root.addInstance("Unit", power);
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

	public void test009() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = root.addInstance("Power", vehicle);
		Generic unit = root.addInstance("Unit", power);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
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

	public void test010() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power1 = root.addInstance("Power", vehicle);
		Generic unit = root.addInstance("Unit", power1);
		Generic car = root.addInstance(Arrays.asList(vehicle), "Car");
		Generic power2 = root.addInstance("Power", car);
		Generic microcar = root.addInstance(Arrays.asList(car), "Microcar");
		int powerValue = 1;
		String unitValue1 = "Watt";
		String unitValue2 = "KWatt";
		Generic v1 = power1.addInstance(powerValue, vehicle);
		Generic v2 = power2.addInstance(Arrays.asList(v1), powerValue, car);
		Generic vUnit1 = unit.addInstance(unitValue1, power1);
		Generic vUnit2 = unit.addInstance(Arrays.asList(vUnit1), unitValue2, power2);
		assert !power1.equals(power2);
		assert power2.inheritsFrom(power1);
		assert v2.inheritsFrom(v1);
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

	public void test011() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myCar = car.addInstance("myCar");
		Generic myCar256 = myCar.setHolder(power, 256);
		assert myCar256.equals(myCar.getHolder(power));
	}

	public void test012() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic maxSpeed = car.addAttribute("MaxSpeed");
		Generic mode = car.addAttribute("Mode");
		Generic myCar = car.addInstance("myCar");
		Generic myCar256 = myCar.setHolder(power, 256);
		myCar.setHolder(maxSpeed, 300);
		myCar.setHolder(mode, "manual");
		assert myCar256.equals(myCar.getHolder(power));
	}

	public void test013() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myCar = car.addInstance("myCar");
		myCar.setHolder(power, 256);
		myCar.setHolder(power, 257);
		catchAndCheckCause(() -> myCar.getHolder(power), AmbiguousSelectionException.class);
	}

	public void test014() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic carDefaultPower = car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		assert carDefaultPower.equals(myCar.getHolder(power));
	}

	public void test015() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power").enablePropertyConstraint();
		car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		Generic myCar256 = myCar.setHolder(power, 256);
		assert myCar256.equals(myCar.getHolder(power));
	}

	public void test016() {
		final Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		Generic myCar256 = myCar.setHolder(power, 256);
		assert myCar256.equals(myCar.getHolder(power, 256));
	}

	public void test017() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic carDefaultPower = car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		myCar.setHolder(power, 256);
		assert carDefaultPower.equals(car.getHolder(power));
	}

	public void test018() {
		final Root engine = new Root();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		car.setHolder(power, 200);
		Generic myCar = car.addInstance("myCar");
		myCar.setHolder(power, 256);
		catchAndCheckCause(() -> myCar.getHolder(power), AmbiguousSelectionException.class);
	}
}
