package org.genericsystem.cache;

import org.testng.annotations.Test;

@Test
public class AdjustMetaTest extends AbstractTest {

	public void test001_AdjustMeta_MetaLevel_metaAttribut_NoComposite() {
		ClientEngine engine = new ClientEngine();
		assert engine == engine.adjustMeta();
	}

	public void test002_AdjustMeta_MetaLevel_metaAttribut_OneComposite() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric metaAttribute = engine.getMetaAttribute();
		ClientGeneric car = engine.addInstance("Car");
		assert engine.adjustMeta(car).equals(metaAttribute);

		assert engine.adjustMeta(car).getValue().equals(metaAttribute.getValue());
		assert metaAttribute.equals(engine.adjustMeta(car));
	}

	public void test003_AdjustMeta_MetaLevel_metaAttribut_TwoComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric metaRelation = engine.getMetaRelation();
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = engine.addInstance("Color");
		assert metaRelation.equals(engine.adjustMeta(car, color));
	}

	public void test004_AdjustMeta_MetaLevel_metaAttribute() {
		ClientEngine engine = new ClientEngine();
		engine.addInstance("Robot");
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = engine.addInstance("Color");
		assert engine.getMetaRelation().equals(engine.adjustMeta(car, color));
		engine.addInstance("CarColor", car, color);
		assert engine.getMetaAttribute().equals(engine.adjustMeta(car));
	}

	public void test005_AdjustMeta_MetaLevel_metaRelation_ThreeComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric metaRelation = engine.getMetaRelation();
		assert metaRelation.equals(engine.adjustMeta(engine, engine));
		assert metaRelation.equals(engine.setInstance(engine.getValue(), engine, engine));
		ClientGeneric car = engine.addInstance("Car");
		ClientGeneric color = engine.addInstance("Color");
		engine.addInstance("CarColor", car, color);
		ClientGeneric finition = engine.addInstance("Finition");
		assert metaRelation.equals(engine.adjustMeta(car, color));
	}

	public void test006_AdjustMeta_TypeLevel_Relation_TwoComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric car = vehicle.addInstance("Car");
		ClientGeneric red = color.addInstance("Red");
		assert vehicleColor == vehicleColor.adjustMeta(car, red);
	}

	public void test007_AdjustMeta_TypeLevel_Relation_TwoComposites_oneCompositeSpecializedByInheritance() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric color2 = engine.addInstance(color, "Color2");
		ClientGeneric car = vehicle.addInstance("Car");
		ClientGeneric red = color2.addInstance("Red");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test008_AdjustMeta_TypeLevel_Relation_TwoComposites_oneCompositeSpecializedByInstanciation() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("VehicleColor", color);
		ClientGeneric red = color.addInstance("red");
		ClientGeneric myVehicle = vehicle.addInstance("myVehicle");
		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle, red));
	}

	public void test009_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecializedByInheritance() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric color2 = engine.addInstance(color, "Color2");
		ClientGeneric car = vehicle2.addInstance("Car");
		ClientGeneric red = color2.addInstance("Red");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test010_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecializedByInstanciation() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric vehicle2 = engine.addInstance("Vehicle2");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle2, red));
	}

	public void test011_AdjustMeta_TypeLevel_Relation_TwoComposites_TwoCompositeSpecialized() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric car = vehicle2.addInstance("Car");
		assert vehicleColor.equals(vehicleColor.adjustMeta(car, red));
	}

	public void test012_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric finition = engine.addInstance("Finition");
		ClientGeneric myVehicle2 = vehicle2.addInstance("myVehicle2");

		assert vehicleColor.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test013_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = engine.addInstance("VehicleColor", vehicle, color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric vehicleColor2 = engine.addInstance(vehicleColor, "VehicleColor2", vehicle2, color);
		ClientGeneric red = color.addInstance("red");
		ClientGeneric finition = engine.addInstance("Finition");
		ClientGeneric myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test014_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("VehicleColor", color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric vehicleColor2 = vehicle2.addAttribute(vehicleColor, "VehicleColor2", color);
		ClientGeneric finition = engine.addInstance("Finition");
		ClientGeneric myVehicle2 = vehicle2.addInstance("myVehicle2");
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test015_AdjustMeta_TypeLevel_Relation_ThreeComposites() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric vehicle = engine.addInstance("Vehicle");
		ClientGeneric color = engine.addInstance("Color");
		ClientGeneric vehicleColor = vehicle.addAttribute("VehicleColor", color);
		ClientGeneric vehicle2 = engine.addInstance(vehicle, "Vehicle2");
		ClientGeneric red = color.addInstance("red");
		ClientGeneric finition = engine.addInstance("Finition");
		ClientGeneric myVehicle2 = vehicle2.addInstance("myVehicle2");
		ClientGeneric vehicleColor2 = vehicle2.addAttribute(vehicleColor, "VehicleColor2", color);
		assert vehicleColor2.equals(vehicleColor.adjustMeta(myVehicle2, red, finition));
	}

	public void test020_AdjustMeta_TypeLevel_Attribute() {
		ClientEngine engine = new ClientEngine();
		ClientGeneric power = engine.addInstance("Power", engine);
		ClientGeneric car = engine.addInstance("Car", engine);
		ClientGeneric carPower = engine.addInstance(power, "carPower", engine);
		assert carPower.equals(power.adjustMeta(car));
	}
}
