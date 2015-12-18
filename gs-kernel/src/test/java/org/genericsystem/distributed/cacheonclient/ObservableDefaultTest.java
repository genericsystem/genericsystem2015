package org.genericsystem.distributed.cacheonclient;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.kernel.Statics;
import org.testng.annotations.Test;

@Test
public class ObservableDefaultTest extends AbstractTest {

	public void basicObservableAttributesTest() throws InterruptedException {
		CocClientEngine engine = new CocClientEngine();

		Generic vehicle = engine.addInstance("vehicle");

		Generic power = vehicle.addAttribute("power");
		Generic car = engine.addInstance(vehicle, "car");

		ObservableList<Generic> carObservableAttributes = car.getObservableAttributes();

		Thread.sleep(100);

		assert carObservableAttributes.contains(power);

		Generic color = engine.addAttribute("color");
		Generic vehicleColor = vehicle.addRelation("vehicleColor", color);

		Thread.sleep(100);

		assert carObservableAttributes.contains(color);
		assert carObservableAttributes.contains(vehicleColor);
		System.out.println(car.getAttributes().toList());
		System.out.println(carObservableAttributes);

	}

	public void test_relationTest10() throws InterruptedException, ExecutionException, TimeoutException {
		CocClientEngine engine = new CocClientEngine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("vehicleColor", color);

		Generic car = engine.addInstance(vehicle, "Car");
		Generic colorMat = engine.addInstance(color, "ColorMat");
		Generic carColorMat = car.addAttribute(vehicleColor, "carColorMat", colorMat);

		Generic myVehicle = vehicle.addInstance("myVehicle");
		Generic red = color.addInstance("red");
		myVehicle.addHolder(vehicleColor, "myVehicleRed", red);

		Generic myCar = car.addInstance("myCar");
		Generic redMat = colorMat.addInstance("redMat");
		myCar.addHolder(carColorMat, "myCarRedMat", redMat);

		Generic vehiclePower = vehicle.addAttribute("power");
		ObservableList<Generic> myVehicleObservableHolders = myVehicle.getObservableHolders(vehiclePower);
		Generic myVehicle125 = myVehicle.addHolder(vehiclePower, "125");

		Thread.sleep(100);

		assert myVehicleObservableHolders.contains(myVehicle125) : myVehicle.getHolders(vehiclePower).info();
		assert myVehicle125.getValue().equals("125");
		assert myVehicle.getAsyncValues(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).contains("125") : myVehicle.getHolders(vehiclePower).info();
		assert myVehicleObservableHolders.size() == 1;

		assert myCar.getAsyncHolders(vehiclePower).get(Statics.SERVER_TIMEOUT, Statics.SERVER_TIMEOUT_UNIT).size() == 0;

	}
}
