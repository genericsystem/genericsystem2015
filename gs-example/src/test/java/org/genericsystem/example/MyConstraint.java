package org.genericsystem.example;

import java.io.Serializable;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.annotations.Components;
import org.genericsystem.api.core.annotations.Dependencies;
import org.genericsystem.api.core.annotations.Meta;
import org.genericsystem.api.core.annotations.Supers;
import org.genericsystem.api.core.annotations.SystemGeneric;
import org.genericsystem.api.core.annotations.value.AxedPropertyClassValue;
import org.genericsystem.api.core.annotations.value.IntValue;
import org.genericsystem.api.core.exceptions.ConstraintViolationException;
import org.genericsystem.defaults.DefaultConfig.MetaAttribute;
import org.genericsystem.defaults.DefaultConfig.SystemMap;
import org.genericsystem.defaults.constraints.Constraint.CheckedConstraint;
import org.genericsystem.kernel.Root;
import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;
import org.testng.annotations.Test;

public class MyConstraint extends AbstractTest {

	@Test
	public void createByApi() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		vehicle.setSystemPropertyValue(InstanceSizeConstraint.class, ApiStatics.NO_POSITION, 0);
		catchAndCheckCause(() -> vehicle.addInstance("myVehicle"), ConstraintViolationException.class);
	}

	public static class InstanceSizeConstraint implements CheckedConstraint<org.genericsystem.kernel.Generic> {

		@Override
		public void check(org.genericsystem.kernel.Generic modified, org.genericsystem.kernel.Generic constraintBase, Serializable value) throws ConstraintViolationException {
			if (constraintBase.getInstances().size() > (int) value)
				throw new ConstraintViolationException("Instance size of " + constraintBase.info() + " is more than " + value) {
				};
		}
	}

	@Test
	public void createByFullAnnot() {
		Engine engine = new Engine(Vehicle.class);
		Generic vehicle = engine.find(Vehicle.class);
		catchAndCheckCause(() -> vehicle.addInstance("myVehicle"), ConstraintViolationException.class);
	}

	@Dependencies({ DefaultInstanceSizeConstraint.class })
	public static class Vehicle {

	}

	@SystemGeneric
	@Meta(MetaAttribute.class)
	@Supers(SystemMap.class)
	@Components(Root.class)
	@AxedPropertyClassValue(propertyClass = InstanceSizeConstraint.class, pos = ApiStatics.NO_POSITION)
	@Dependencies({ DefaultValue.class })
	public static class DefaultInstanceSizeConstraint {
	}

	@SystemGeneric
	@Meta(DefaultInstanceSizeConstraint.class)
	@Components(Vehicle.class)
	@IntValue(0)
	public static class DefaultValue {
	}

	// @Test
	// public void createBySimpleFullAnnot() {
	// Engine engine = new Engine(Car.class);
	// Generic vehicle = engine.find(Car.class);
	// catchAndCheckCause(() -> vehicle.addInstance("myCar"), ConstraintViolationException.class);
	// }
	//
	// @Constraint(propertyClass = InstanceSizeConstraint.class, pos = ApiStatics.NO_POSITION)
	// public static class Car {
	//
	// }

}
