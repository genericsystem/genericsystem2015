package org.genericsystem.cache;

import java.util.List;
import java.util.stream.Collectors;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.defaults.exceptions.RequiredConstraintViolationException;
import org.testng.annotations.Test;

@Test
public class RequiredConstraintTest extends AbstractTest {

	public void test00_Inheritance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = engine.addAttribute("Power");

		Generic v235 = car.addHolder(power, 235);

		assert car.getHolders(power).contains(v235);
	}

	public void test01_Inheritance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = engine.addAttribute("Power", engine);
		Generic unit = engine.addInstance("Unit");

		Generic v235 = car.addHolder(power, 235, unit);

		assert car.getHolders(power).contains(v235);
		assert unit.getHolders(power).contains(v235);

	}

	public void test000_Inheritance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");

		Generic myBmw = car.addInstance("myBmw");

		Generic v235 = myBmw.addHolder(power, 235);

		assert myBmw.getHolders(power).contains(v235);

	}

	public void test001_Inheritance() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic unit = engine.addInstance("Unit");
		Generic power = car.addAttribute("Power", unit);
		Generic myBmw = car.addInstance("myBmw");

		Generic kw = unit.addInstance("kw");

		Generic v235 = myBmw.addHolder(power, 235, kw);

		assert myBmw.getHolders(power).contains(v235);
		assert kw.getHolders(power).contains(v235);

	}

	public void test001_enableRequired() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		power.enableRequiredConstraint(ApiStatics.BASE_POSITION);

		assert power.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		assert !car.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		power.disableRequiredConstraint(ApiStatics.BASE_POSITION);
		assert !car.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		List<Generic> list = power.getHolders(engine).stream().collect(Collectors.toList());

		assert !power.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION) : power.getHolders(engine).stream().map(x -> x.info()).collect(Collectors.toList()).toString();
	}

	public void test002_removeAttribute() {
		Engine engine = new Engine();
		Generic car = engine.addInstance("Car");
		Generic power = car.addAttribute("Power");
		power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		Generic myBmw = car.addInstance("myBmw");
		Generic v236 = myBmw.addHolder(power, 236);
		assert power.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		Cache cache = engine.getCurrentCache();
		assert myBmw.getHolders(power).contains(v236);
		cache.flush();
		v236.remove();
		catchAndCheckCause(() -> cache.flush(), RequiredConstraintViolationException.class);

	}

	public void test003_removeAttribute_inherintings() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic power = vehicle.addAttribute("Power");

		power.enableRequiredConstraint(ApiStatics.BASE_POSITION);
		Generic myBmw = car.addInstance("myBmw");
		Generic v236 = myBmw.addHolder(power, 236);
		assert power.isRequiredConstraintEnabled(ApiStatics.BASE_POSITION);
		Cache cache = engine.getCurrentCache();
		// power.getComponents().stream().forEach(x -> System.out.println(x.detailedInfo()));
		cache.flush();
		v236.remove();
		catchAndCheckCause(() -> cache.flush(), RequiredConstraintViolationException.class);
	}
	//
	// public void test004_removeAttr() {
	// Engine engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic car = engine.addInstance(vehicle, "Car");
	//
	// }

	// public void test004_addAttribute() {
	// Engine engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic power = vehicle.addAttribute("Power");
	//
	// power.enableRequiredConstraint(Statics.BASE_POSITION);
	// Generic myBmw = vehicle.addInstance("myBmw");
	// Cache cache = engine.getCurrentCache();
	// catchAndCheckCause(() -> cache.flush(), RequiredConstraintViolationException.class);
	//
	// }

	// public void test001_enableRequiredConstraint_addInstance() {
	// Engine engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic color = engine.addInstance("Color");
	// Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
	// vehicleColor.enableRequiredConstraint(Statics.BASE_POSITION);
	// assert vehicleColor.isRequiredConstraintEnabled(Statics.BASE_POSITION);
	// engine.getCurrentCache().flush();
	// }

	// TODO test à décommenter et faire fonctionner
	// public void test002_enableRequiredConstraint_addInstance() {
	// Engine engine = new Engine();
	// Generic vehicle = engine.addInstance("Vehicle");
	// Generic color = engine.addInstance("Color");
	// Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);
	// vehicleColor.enableRequiredConstraint(Statics.BASE_POSITION);
	// assert vehicleColor.isRequiredConstraintEnabled(Statics.BASE_POSITION);
	// engine.getCurrentCache().flush();
	// new RollbackCatcher() {
	// @Override
	// public void intercept() {
	// vehicle.addInstance("myVehicle");
	// engine.getCurrentCache().flush();
	// }
	// }.assertIsCausedBy(RequiredConstraintViolationException.class);
	// }

	// public void test002_enableSingularConstraint_addInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex myVehicle = vehicle.addInstance("myVehicle");
	// Vertex yourVehicle = vehicle.addInstance("yourVehicle");
	// Vertex color = engine.addInstance("Color");
	// Vertex red = color.addInstance("red");
	// Vertex vehicleColor = vehicle.addAttribute("vehicleColor", color);
	// vehicleColor.enableSingularConstraint(Statics.BASE_POSITION);
	// assert vehicleColor.isSingularConstraintEnabled(Statics.BASE_POSITION);
	// myVehicle.addHolder(vehicleColor, "vehicleRed", red);
	// yourVehicle.addHolder(vehicleColor, "vehicleRed", red);
	// }
	//
	// public void test003_enableSingularConstraint_addDefaultInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex color = engine.addInstance("Color");
	// Vertex red = color.addInstance("red");
	// Vertex yellow = color.addInstance("yellow");
	// Vertex vehicleColor = vehicle.addAttribute("vehicleColor", color);
	// vehicleColor.enableSingularConstraint(Statics.BASE_POSITION);
	// assert vehicleColor.isSingularConstraintEnabled(Statics.BASE_POSITION);
	// vehicle.addHolder(vehicleColor, "vehicleRed", red);
	// new RollbackCatcher() {
	// @Override
	// public void intercept() {
	// vehicle.addHolder(vehicleColor, "vehicleYellow", yellow);
	// }
	// }.assertIsCausedBy(SingularConstraintViolationException.class);
	// }
	//
	// public void test001_enableSingularConstraint_ternaryRelation() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex myVehicle = vehicle.addInstance("myVehicle");
	// Vertex color = engine.addInstance("Color");
	// Vertex time = engine.addInstance("Time");
	// Vertex red = color.addInstance("red");
	// Vertex today = time.addInstance("today");
	// Vertex yesterday = time.addInstance("yesterday");
	// Vertex vehicleColor = vehicle.addAttribute("vehicleColor", color);
	// vehicleColor.enableSingularConstraint(Statics.BASE_POSITION);
	// assert vehicleColor.isSingularConstraintEnabled(Statics.BASE_POSITION);
	// myVehicle.addHolder(vehicleColor, "vehicleRedToday", red, today);
	// new RollbackCatcher() {
	// @Override
	// public void intercept() {
	// myVehicle.addHolder(vehicleColor, "vehicleRedYesterday", red, yesterday);
	// }
	// }.assertIsCausedBy(SingularConstraintViolationException.class);
	// }

	// public void test002_enablePropertyConstraint_addInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex power = engine.addInstance("Power", vehicle);
	// Vertex subPower = engine.addInstance(power, "SubPower", vehicle);
	// assert subPower.inheritsFrom(power);
	// power.enablePropertyConstraint();
	// assert subPower.isPropertyConstraintEnabled();
	// subPower.addInstance("123", vehicle);
	// new RollbackCatcher() {
	//
	// @Override
	// public void intercept() {
	// subPower.addInstance("126", vehicle);
	// }
	// }.assertIsCausedBy(ExistsException.class);
	// }
	//
	// public void test003_enablePropertyConstraint_addInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex car = engine.addInstance(vehicle, "Car");
	// Vertex power = engine.addInstance("Power", vehicle);
	// Vertex subPower = engine.addInstance(power, "Power", car);
	// assert subPower.inheritsFrom(power);
	// power.enablePropertyConstraint();
	// assert subPower.isPropertyConstraintEnabled();
	// subPower.addInstance("123", car);
	// new RollbackCatcher() {
	//
	// @Override
	// public void intercept() {
	// subPower.addInstance("126", car);
	// }
	// }.assertIsCausedBy(ExistsException.class);
	// }
	//
	// public void test001_enablePropertyConstraint_setInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex power = engine.addInstance("Power", vehicle);
	// power.enablePropertyConstraint();
	// assert power.isPropertyConstraintEnabled();
	// power.setInstance("123", vehicle);
	// power.setInstance("126", vehicle);
	// assert power.getInstances().size() == 1;
	// power.getInstances().forEach(x -> x.getValue().equals("126"));
	// }
	//
	// public void test001_disablePropertyConstraint_setInstance() {
	// Root engine = new Root();
	// Vertex vehicle = engine.addInstance("Vehicle");
	// Vertex power = engine.addInstance("Power", vehicle);
	// power.enablePropertyConstraint();
	// assert power.isPropertyConstraintEnabled();
	// power.setInstance("123", vehicle);
	// power.setInstance("126", vehicle);
	// assert power.getInstances().size() == 1;
	// power.getInstances().forEach(x -> x.getValue().equals("126"));
	// power.disablePropertyConstraint();
	// assert !power.isPropertyConstraintEnabled();
	// power.setInstance("123", vehicle);
	// assert power.getInstances().size() == 2;
	// }

}
