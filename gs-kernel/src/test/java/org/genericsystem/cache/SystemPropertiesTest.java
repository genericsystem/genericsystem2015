package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class SystemPropertiesTest extends AbstractClassicTest {

	public void test001_enableConstraint() {
		ClientEngine Engine = new ClientEngine();
		ClientGeneric vehicle = Engine.addInstance("Vehicle");
		ClientGeneric power = Engine.addInstance("Power", vehicle);
		power.enablePropertyConstraint();
		power.enablePropertyConstraint();
		power.enableReferentialIntegrity(0);
		assert power.isPropertyConstraintEnabled();
		assert power.isReferentialIntegrityEnabled(0);
		power.disablePropertyConstraint();
		power.disableReferentialIntegrity(0);
		power.disablePropertyConstraint();
		assert !power.isPropertyConstraintEnabled();
		assert !power.isReferentialIntegrityEnabled(0);
		power.enablePropertyConstraint();
		power.enableReferentialIntegrity(0);
		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();
		assert power.isReferentialIntegrityEnabled(0);
	}

	public void test002_inheritedConstraint() {
		ClientEngine Engine = new ClientEngine();
		ClientGeneric vehicle = Engine.addInstance("Vehicle");
		ClientGeneric car = Engine.addInstance(vehicle, "Car");
		ClientGeneric power = Engine.addInstance("Power", vehicle);
		ClientGeneric carPower = Engine.addInstance("Power", car);
		power.enableSingularConstraint(0);
		power.enablePropertyConstraint();
		power.enableReferentialIntegrity(0);
		power.enablePropertyConstraint();
		assert carPower.isPropertyConstraintEnabled();
		assert carPower.isReferentialIntegrityEnabled(0);
		power.disablePropertyConstraint();
		power.disableReferentialIntegrity(0);
		power.disablePropertyConstraint();
		assert !carPower.isPropertyConstraintEnabled();
		assert !carPower.isReferentialIntegrityEnabled(0);
		power.enablePropertyConstraint();
		power.enableReferentialIntegrity(0);
		power.enablePropertyConstraint();
		assert carPower.isPropertyConstraintEnabled();
		assert carPower.isReferentialIntegrityEnabled(0);
	}

	public void test003_inheritedModifiedConstraint() {
		ClientEngine Engine = new ClientEngine();
		ClientGeneric vehicle = Engine.addInstance("Vehicle");
		ClientGeneric car = Engine.addInstance(vehicle, "Car");
		ClientGeneric power = Engine.addInstance("Power", vehicle);
		ClientGeneric carPower = Engine.addInstance("Power", car);
		power.enablePropertyConstraint();
		assert carPower.isPropertyConstraintEnabled();
		carPower.disablePropertyConstraint();
		assert !carPower.isPropertyConstraintEnabled();
		assert power.isPropertyConstraintEnabled();
		power.enablePropertyConstraint();
		assert !carPower.isPropertyConstraintEnabled();
		assert power.isPropertyConstraintEnabled();
		power.disablePropertyConstraint();
		assert !power.isPropertyConstraintEnabled();

	}

}
