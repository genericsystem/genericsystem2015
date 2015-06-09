package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class AdjustMetaTest extends AbstractTest {

	public void test001_AdjustMeta_MetaLevel_metaAttribut_NoComposite() {
		Engine engine = new Engine();
		assert engine == engine.adjustMeta();
	}

	public void test002_AdjustMeta_MetaLevel_metaAttribut_OneComposite() {
		Engine engine = new Engine();
		Generic metaAttribute = engine.getMetaAttribute();
		Generic car = engine.addInstance("Car");
		assert engine.adjustMeta(car).equals(metaAttribute);

		assert engine.adjustMeta(car).getValue().equals(metaAttribute.getValue());
		assert metaAttribute.equals(engine.adjustMeta(car));
	}

	public void test003_AdjustMeta_MetaLevel_metaAttribut_TwoComposites() {
		Engine engine = new Engine();
		Generic metaRelation = engine.getMetaRelation();
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		assert metaRelation.equals(engine.adjustMeta(car, color));
	}

	public void test004_AdjustMeta_MetaLevel_metaAttribute() {
		Engine engine = new Engine();
		engine.addInstance("Robot");
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		assert engine.getMetaRelation().equals(engine.adjustMeta(car, color));
		engine.addInstance("CarColor", car, color);
		assert engine.getMetaAttribute().equals(engine.adjustMeta(car));
	}

	public void test005_AdjustMeta_MetaLevel_metaRelation_ThreeComposites() {
		Engine engine = new Engine();
		Generic metaRelation = engine.getMetaRelation();
		assert metaRelation.equals(engine.adjustMeta(engine, engine));
		assert metaRelation.equals(engine.setInstance(engine.getValue(), engine, engine));
		Generic car = engine.addInstance("Car");
		Generic color = engine.addInstance("Color");
		engine.addInstance("CarColor", car, color);
		Generic finition = engine.addInstance("Finition");
		assert metaRelation.equals(engine.adjustMeta(car, color));
	}

	public void test006_AdjustMeta_TypeLevel_Relation_TwoComposites() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic car = vehicle.addInstance("Car");
		Generic red = color.addInstance("Red");
		assert vehicleColor == vehicleColor.adjustMeta(car, red);
	}

	public void test007_AdjustMeta_TypeLevel_Relation_TwoComposites_oneCompositeSpecializedByInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic color2 = engine.addInstance(color, "Color2");
		Generic car = vehicle.addInstance("Car");
		Generic red = color2.addInstance("Red");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test008_AdjustMeta_TypeLevel_Relation_TwoComposites_oneCompositeSpecializedByInstanciation() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("VehicleColor", color);
		Generic red = color.addInstance("red");
		Generic myVehicle = vehicle.addInstance("myVehicle");
		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle, red));
	}

	public void test009_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecializedByInheritance() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic color2 = engine.addInstance(color, "Color2");
		Generic car = vehicle2.addInstance("Car");
		Generic red = color2.addInstance("Red");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test010_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecializedByInstanciation() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic vehicle2 = engine.addInstance("Vehicle2");
		Generic red = color.addInstance("red");
		Generic myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle2, red));
	}

	public void test011_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecialized() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic red = color.addInstance("red");
		Generic car = vehicle2.addInstance("Car");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test012_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic red = color.addInstance("red");
		Generic finition = engine.addInstance("Finition");
		Generic myVehicle2 = vehicle2.addInstance("myVehicle2");

		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test013_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic vehicleColor2 = engine.addInstance(vehicleColor, "VehicleColor2", vehicle2, color);
		Generic red = color.addInstance("red");
		Generic finition = engine.addInstance("Finition");
		Generic myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test014_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("VehicleColor", color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic red = color.addInstance("red");
		Generic vehicleColor2 = vehicle2.addAttribute(vehicleColor, "VehicleColor2", color);
		Generic finition = engine.addInstance("Finition");
		Generic myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test015_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		Engine engine = new Engine();
		Generic vehicle = engine.addInstance("Vehicle");
		Generic color = engine.addInstance("Color");
		Generic vehicleColor = vehicle.addAttribute("VehicleColor", color);
		Generic vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		Generic red = color.addInstance("red");
		Generic finition = engine.addInstance("Finition");
		Generic myVehicle2 = vehicle2.addInstance("myVehicle2");
		Generic vehicleColor2 = vehicle2.addAttribute(vehicleColor, "VehicleColor2", color);
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test020_AdjustMeta_TypeLevel_Attribute() {
		Engine engine = new Engine();
		Generic power = engine.addInstance("Power", engine);
		Generic car = engine.addInstance("Car", engine);
		Generic carPower = engine.addInstance(power, "carPower", engine);
		assert carPower.equals(power.adjustMeta(car));
	}
}
