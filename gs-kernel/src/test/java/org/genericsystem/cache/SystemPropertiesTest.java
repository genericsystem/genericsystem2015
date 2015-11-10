package org.genericsystem.cache;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Engine;
import org.testng.annotations.Test;

@Test
public class SystemPropertiesTest extends AbstractTest {

	public void test001_enableConstraint() {
		Engine ServerEngine = new Engine();
		Generic vehicle = ServerEngine.addInstance("Vehicle");
		Generic power = ServerEngine.addInstance("Power", vehicle);
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
		Engine ServerEngine = new Engine();
		Generic vehicle = ServerEngine.addInstance("Vehicle");
		Generic car = ServerEngine.addInstance(vehicle, "Car");
		Generic power = ServerEngine.addInstance("Power", vehicle);
		Generic carPower = ServerEngine.addInstance("Power", car);
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
		Engine ServerEngine = new Engine();
		Generic vehicle = ServerEngine.addInstance("Vehicle");
		Generic car = ServerEngine.addInstance(vehicle, "Car");
		Generic power = ServerEngine.addInstance("Power", vehicle);
		Generic carPower = ServerEngine.addInstance("Power", car);
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
