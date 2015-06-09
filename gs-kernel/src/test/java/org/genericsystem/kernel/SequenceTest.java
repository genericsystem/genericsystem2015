package org.genericsystem.kernel;

import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.constraints.InstanceValueGenerator;
import org.genericsystem.defaults.IntSequenceGenerator;
import org.genericsystem.defaults.IntSequenceGenerator.StringSequenceGenerator;
import org.testng.annotations.Test;

@Test
public class SequenceTest extends AbstractTest {

	public void testFindSequence() {
		Root root = new Root();
		Generic sequence = root.getSequence();
		assert sequence != null;
		assert sequence.getMeta() == root.getMetaAttribute();
		assert sequence.getComponents().contains(root);
	}

	public void testStringAutoIncrementGenerator() {
		// Root root = new Root(Car.class);
		// Generic car = root.find(Car.class);
		// Generic myBmw = car.addGenerateInstance();
		// assert myBmw.getValue() instanceof String;
		// assert ((String) myBmw.getValue()).contains(Car.class.getSimpleName());

		Root root = new Root(Car.class);
		Generic car = root.find(Car.class);
		Generic myBmw = car.addInstance(null);
		assert myBmw.getValue() instanceof String;
		assert ((String) myBmw.getValue()).contains(Car.class.getSimpleName());
	}

	public void testIntAutoIncrementGenerator() {
		Root root = new Root(CarInt.class);
		Generic car = root.find(CarInt.class);
		Generic myBmw = car.addInstance(null);
		assert myBmw.getValue() instanceof Integer;
		assert ((Integer) myBmw.getValue()) == 0;
	}

	public void testHolderIntAutoIncrementGenerator() {
		Root root = new Root(Id.class);
		Generic id = root.find(Id.class);
		Generic vehicle = root.find(Vehicle.class);
		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic myVehicleId = id.addInstance(null, myVehicle);
		assert myVehicleId.getValue() instanceof Integer;
		assert ((Integer) myVehicleId.getValue()) == 0;
		Generic myVehicleId2 = id.addInstance(null, myVehicle);
		assert ((Integer) myVehicleId2.getValue()) == 1;
	}

	@SystemGeneric
	@InstanceValueGenerator(StringSequenceGenerator.class)
	public static class Car {

	}

	@SystemGeneric
	@InstanceValueGenerator(IntSequenceGenerator.class)
	public static class CarInt {

	}

	@SystemGeneric
	public static class Vehicle {

	}

	@SystemGeneric
	@Components(Vehicle.class)
	@InstanceValueGenerator(IntSequenceGenerator.class)
	public static class Id {

	}

}
