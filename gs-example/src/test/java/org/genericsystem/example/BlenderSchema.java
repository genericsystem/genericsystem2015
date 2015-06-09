package org.genericsystem.example;

import org.genericsystem.mutability.Engine;
import org.genericsystem.mutability.Generic;

public class BlenderSchema {

	public void buildSchema() {
		Engine engine = new Engine();

		Generic vehicle = engine.addInstance("Vehicle");
		Generic car = engine.addInstance(vehicle, "Car");
		Generic bike = engine.addInstance(vehicle, "Bike");
		Generic color = engine.addInstance("Color");
		Generic power = vehicle.addAttribute("Power");
		Generic unit = power.addAttribute("Unit");
		Generic carColor = car.addRelation("CarColor", color);

		Generic myAudi = car.addInstance("myAudi");
		Generic myBmw = car.addInstance("myBmw");
		Generic myYamaha = bike.addInstance("myYamaha");

		Generic red = color.addInstance("red");
		Generic blue = color.addInstance("blue");
		Generic yellow = color.addInstance("yellow");

		Generic v50 = myYamaha.addHolder(power, 50);
		Generic v233 = myBmw.addHolder(power, 233);
		Generic v333 = myAudi.addHolder(power, 333);

		Generic hp = v333.addHolder(unit, "hp");
		Generic kw = v50.addHolder(unit, "kw");

		Generic myAudiBlue = myAudi.addLink(carColor, "myAudiBlue", blue);
		Generic myAudiRed = myAudi.addLink(carColor, "myAudiRed", red);
		Generic myBmwBlue = myBmw.addLink(carColor, "myAudiBlue", blue);
	}

}
