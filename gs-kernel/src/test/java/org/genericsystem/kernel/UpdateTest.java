package org.genericsystem.kernel;

import java.util.Collections;
import org.genericsystem.api.core.exceptions.MetaRuleConstraintViolationException;
import org.genericsystem.defaults.exceptions.PropertyConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class UpdateTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		assert "Car".equals(car.getValue());
		Generic newCar = car.update("NewCar");
		assert !car.isAlive();
		assert "NewCar".equals(newCar.getValue());
	}

	public void test002() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("MyBmw");
		myBmw.addHolder(power, "myBmwV233");

		assert myBmw.getMeta().equals(car);
		Generic newCar = car.updateValue("NewCar");
		assert newCar.getInstances().stream().allMatch(x -> "MyBmw".equals(x.getValue()));
		assert newCar.getInstances().stream().allMatch(x -> x.getHolders(power).stream().allMatch(y -> "myBmwV233".equals(y.getValue())));
		assert !myBmw.isAlive();
		assert !car.isAlive();
		assert root.getInstances().contains(newCar);
		assert root.getInstances().size() == 1;
		assert car.getMeta().equals(root);
	}

	public void test003() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("MyBmw");
		Generic myBmw233 = myBmw.addHolder(power, 233);

		assert myBmw.getComposites().contains(myBmw233);
		assert myBmw.getComposites().size() == 1;
		assert myBmw233.getValue().equals(233);

		Generic myBmw455 = myBmw233.updateValue(455);

		assert !myBmw233.isAlive();
		assert myBmw.getComposites().contains(myBmw455);
		assert myBmw.getComposites().size() == 1;
		assert myBmw455.getValue().equals(455);
	}

	public void test004() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");

		assert car.getSupers().contains(vehicle);
		Generic otherVehicle = root.addInstance("OtherVehicle");
		Generic otherCar = car.updateSupers(otherVehicle);

		assert !car.isAlive();
		assert vehicle.isAlive();

		assert car.getSupers().contains(vehicle);
		assert vehicle.getInheritings().size() == 0;

		assert otherCar.isAlive();
		assert otherCar.getSupers().contains(otherVehicle);

		assert otherVehicle.getInheritings().contains(otherCar);
		assert otherVehicle.getInheritings().size() == 1;

	}

	public void test005() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("MyBmw");
		myBmw.addHolder(power, 233);
		Generic powerType = root.addInstance("PowerType");
		catchAndCheckCause(() -> power.update("carPower", powerType), MetaRuleConstraintViolationException.class);
	}

	public void test006() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw233 = myBmw.addHolder(power, 233);

		Generic newVehicle = vehicle.update("NewVehicle");

		assert newVehicle.isAlive();
		assert !vehicle.isAlive();
		assert root.getInstances().contains(newVehicle);
		assert root.getInstances().contains(newVehicle.getInheritings().first());
		assert root.getInstances().size() == 2;

		assert newVehicle.getInheritings().stream().allMatch(x -> "Car".equals(x.getValue()));
		assert newVehicle.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> "myBmw".equals(y.getValue())));
		assert newVehicle.getInheritings().stream().allMatch(x -> x.getInstances().stream().allMatch(y -> y.getHolders(power).stream().allMatch(z -> z.getValue().equals(233))));

		assert myBmw233.getValue().equals(233);
	}

	public void test007() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		vehicle.addInstance("myVehicle");
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");

		Generic newCar = root.getCurrentCache().merge(car, Collections.emptyList(), "Vehicle", Collections.emptyList());

		assert newCar.equals(vehicle) : newCar.info();
		assert !car.isAlive();
		assert !myBmw.isAlive();

		assert vehicle.getInstance("myBmw") != null;
		assert vehicle.getInstance("myVehicle") != null : newCar.getInstances().info();
		assert vehicle.getInstances().size() == 2;
	}

	public void test008() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic power = vehicle.addAttribute("Power");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic myVehicle233 = myVehicle.addHolder(power, 233);

		Generic car = root.addInstance("Car");
		assert !root.getCurrentCache().computeDependencies(car).contains(power);
		Generic carPower = car.addAttribute("Power");
		Generic myBmw = car.addInstance("myBmw");
		Generic myBmw233 = myBmw.addHolder(carPower, 233);

		Generic newCar = root.getCurrentCache().merge(car, Collections.emptyList(), "Vehicle", Collections.emptyList());
		assert power.isAlive();
		assert !carPower.isAlive();

		assert newCar.equals(vehicle);
		assert !car.isAlive();
		assert !myBmw.isAlive();

		assert vehicle.getInstance("myBmw") != null;
		assert vehicle.getInstance("myVehicle") != null;
		assert vehicle.getInstances().size() == 2;

		myBmw = vehicle.getInstance("myBmw");
		myVehicle = vehicle.getInstance("myVehicle");
		assert vehicle.isAlive();
		assert power.isAlive();
		assert vehicle.getAttributes().contains(power);
		assert myVehicle233.isAlive();

		assert !carPower.isAlive();
		assert !myBmw233.isAlive();

		assert !myBmw.getComposites().isEmpty();
		assert !myVehicle.getHolders(power).isEmpty();
		assert !myBmw.getHolders(power).isEmpty();

		assert myVehicle.getHolders(power).first().getValue().equals(233);
		assert myVehicle.getHolders(power).first().getValue().equals(233);
	}

	public void test009() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");

		Generic color = root.addInstance("Color");
		Generic red = color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		Generic carColor = car.addRelation("carColor", color);
		carColor.enablePropertyConstraint();

		Generic myBmwRed = carColor.addInstance("myBmwRed", myBmw, red);
		carColor.addInstance("myBmwYellow", myBmw, yellow);
		catchAndCheckCause(() -> myBmwRed.update("myBmwRed", myBmw, yellow), PropertyConstraintViolationException.class);
	}

	public void test010() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic myBmw = car.addInstance("myBmw");

		Generic color = root.addInstance("Color");
		color.addInstance("red");
		Generic yellow = color.addInstance("yellow");

		Generic carColor = car.addRelation("carColor", color);
		carColor.enablePropertyConstraint();

		carColor.addInstance("myBmwYellow", myBmw, yellow);
		catchAndCheckCause(() -> carColor.addInstance("myBmwRed", myBmw, yellow), PropertyConstraintViolationException.class);
	}
}
