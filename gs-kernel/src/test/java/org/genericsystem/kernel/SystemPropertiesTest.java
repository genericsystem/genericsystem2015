package org.genericsystem.kernel;

import org.testng.annotations.Test;

@Test
public class SystemPropertiesTest extends AbstractTest {

	public void test001() {
		Root root = new Root();
		Generic car = root.addInstance("Car");
		Generic power = root.addInstance("Power", car);
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

	public void test002() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", vehicle);
		Generic carPower = root.addInstance("Power", car);
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

	public void test003() {
		Root root = new Root();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = root.addInstance("Power", vehicle);
		Generic carPower = root.addInstance("Power", car);
		power.enablePropertyConstraint();
		assert power.isPropertyConstraintEnabled();
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
