package org.genericsystem.distributed.cacheonclient;

import org.genericsystem.common.Generic;
import org.genericsystem.remote.ClientEngine;
import org.testng.annotations.Test;

@Test
public class SystemPropertiesTest extends AbstractTest {

	public void test001_enableConstraint() {
		ClientEngine Engine = new ClientEngine();
		Generic vehicle = Engine.addInstance("Vehicle");
		Generic power = Engine.addInstance("Power", vehicle);
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
		Generic vehicle = Engine.addInstance("Vehicle");
		Generic car = Engine.addInstance(vehicle, "Car");
		Generic power = Engine.addInstance("Power", vehicle);
		Generic carPower = Engine.addInstance("Power", car);
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
		Generic vehicle = Engine.addInstance("Vehicle");
		Generic car = Engine.addInstance(vehicle, "Car");
		Generic power = Engine.addInstance("Power", vehicle);
		Generic carPower = Engine.addInstance("Power", car);
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
