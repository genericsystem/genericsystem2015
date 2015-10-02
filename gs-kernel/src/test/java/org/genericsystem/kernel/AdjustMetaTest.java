package org.genericsystem.kernel;

import java.util.Arrays;
import java.util.Collections;
import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.api.core.exceptions.ExistsException;
import org.genericsystem.common.Generic;
import org.testng.annotations.Test;

@Test
public class AdjustMetaTest extends AbstractTest {

	public void test001() {
		LightServerEngine root = new LightServerEngine();
		Generic metaAttribute = root.getMetaAttribute();
		assert metaAttribute.equals(root.adjustMeta(root));
		assert metaAttribute == root.adjustMeta(root);
		assert root.getMetaRelation().equals(root.adjustMeta(root, root));
		assert root.getMetaRelation() == root.setInstance(root.getValue(), root, root);
		Generic metaTernaryRelation = root.setInstance(root.getValue(), root, root, root);
		assert root.getCurrentCache().getMeta(3).equals(metaTernaryRelation);
		assert metaAttribute.getInheritings().first() == root.adjustMeta(root, root);
	}

	public void test002() {
		LightServerEngine root = new LightServerEngine();
		assert root == root.adjustMeta(Collections.emptyList());
	}

	public void test003() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		assert root.getMetaAttribute() == root.adjustMeta(car);
	}

	public void test004() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		assert root.getMetaRelation() == root.adjustMeta(car, color);
	}

	public void test005() {
		LightServerEngine root = new LightServerEngine();
		assert root.getMetaAttribute() == root.adjustMeta(root);
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		root.addInstance("CarColor", car, color);
		Generic design = root.addInstance("Design");
		Generic adjustMeta = root.adjustMeta(Arrays.asList(car, color, design));
		assert root.getMetaRelation() == adjustMeta : adjustMeta.info();

	}

	public void test006() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = root.addInstance("CarColor", car, color);
		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("Red");
		assert carColor == carColor.adjustMeta(Arrays.asList(myBmw, red)) : root.adjustMeta(Arrays.asList(myBmw, red));
	}

	public void test007() {
		LightServerEngine root = new LightServerEngine();
		assert root.getMetaAttribute().equalsRegardlessSupers(root.getMetaAttribute(), root.getMetaAttribute().getValue(), Collections.singletonList(root));

		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = root.addInstance("CarColor", car, color);
		Generic red = root.addInstance(color, "red");
		Generic myBmw = car.addInstance("myBmw");
		assert carColor == carColor.adjustMeta(myBmw, red);
	}

	public void test008() {
		LightServerEngine root = new LightServerEngine();
		Generic car = root.addInstance("Car");
		Generic color = root.addInstance("Color");
		Generic carColor = root.addInstance("CarColor", car, color);
		Generic red = color.addInstance("red");
		Generic myBmw = car.addInstance("myBmw");
		assert carColor == carColor.adjustMeta(myBmw, red);
	}

	public void test009() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic car = root.addInstance(vehicle, "Car");
		Generic red = root.addInstance(color, "red");
		Generic myBmw = car.addInstance("myBmw");
		assert vehicleColor == vehicleColor.adjustMeta(myBmw, red);
	}

	public void test010() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic car = root.addInstance(vehicle, "Car");
		Generic red = color.addInstance("red");
		Generic myBmw = car.addInstance("myBmw");
		assert vehicleColor == vehicleColor.adjustMeta(myBmw, red);
	}

	public void test011() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);

		Generic car = root.addInstance(vehicle, "Car");
		Generic carColor = root.addInstance(vehicleColor, "CarColor", car, color);
		Generic red = color.addInstance("red");
		Generic design = root.addInstance("Design");

		assert carColor == vehicleColor.adjustMeta(car, red, design);
	}

	public void test012() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = root.addInstance("VehicleColor", vehicle, color);
		Generic car = root.addInstance(vehicle, "Car");
		Generic carColor = root.addInstance(vehicleColor, "CarColor", car, color);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		assert carColor == vehicleColor.adjustMeta(myBmw, red);
		assert vehicleColor == vehicleColor.adjustMeta(myVehicle, red);
	}

	public void test013() {
		LightServerEngine root = new LightServerEngine();
		// TODO power n'est pas un attribut comme ça ? carPower n'a aucun rapport avec car ?
		Generic power = root.addInstance("Power", root);
		Generic car = root.addInstance("Car", root);
		Generic carPower = root.addInstance(power, "carPower", root);
		assert carPower.equals(power.adjustMeta(car));
	}

	/**
	 * Others tests
	 */

	public void test014() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic bike = root.addInstance(vehicle, "Bike");

		car.addInstance("myBmw");
		bike.addInstance("myBmw");

		assert vehicle.getInstance("myBmw") == null;
	}

	public void test015() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic bike = root.addInstance(vehicle, "Bike");

		Generic myBmw = car.addInstance("myBmw");
		assert myBmw.getMeta().equals(car);
		assert myBmw.equals(car.getInstance("myBmw"));
		bike.addInstance("myBmw");

		catchAndCheckCause(() -> bike.addInstance("myBmw"), ExistsException.class);

	}

	public void test016() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic bike = root.addInstance(vehicle, "Bike");
		Generic vtt = root.addInstance(bike, "VTT");

		Generic vehicleInstance = vehicle.addInstance("instance");
		Generic bikeInstance = bike.addInstance("instanceBike");
		Generic vttInstance = vtt.addInstance("instance");
		assert vehicle.getInstance("instance").equals(vehicleInstance);
		assert bike.getInstance("instanceBike").equals(bikeInstance);
		assert vtt.getInstance("instance").equals(vttInstance);
	}

	public void test017() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic color = root.addInstance("Color");
		Generic vehicleColor = vehicle.addRelation("VehicleColor", color);
		Generic carColor = car.addRelation(vehicleColor, "CarColor", color);
		assert carColor.inheritsFrom(vehicleColor);

		Generic myBmw = car.addInstance("myBmw");
		Generic red = color.addInstance("red");
		assert myBmw.getRelations(ApiStatics.BASE_POSITION).contains(carColor);
		assert !myBmw.getRelations(ApiStatics.TARGET_POSITION).contains(carColor);
		Generic myBmwRed = myBmw.addLink(vehicleColor, "myBmwRed", red);
		assert carColor.equals(myBmwRed.getMeta());
	}

	public void test018() {
		LightServerEngine root = new LightServerEngine();
		Generic vehicle = root.addInstance("Vehicle");
		Generic car = root.addInstance(vehicle, "Car");
		Generic power = vehicle.addAttribute("Power");

		Generic myBmw = car.addInstance("myBmw");
		Generic holder = myBmw.addHolder(power, 235);
		assert holder.getMeta().equals(power);
		Generic carPower = car.addAttribute(power, "CarPower");
		assert !holder.isAlive();
		assert carPower.equals(myBmw.getHolders(power).first().getMeta());
	}

	public void test019() {
		LightServerEngine root = new LightServerEngine();
		Generic metaAttribute = root.getMetaAttribute();
		Generic systemMap = root.getMap();
		assert systemMap.getMeta().equals(metaAttribute);
	}
}
