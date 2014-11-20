class Engine() {	
	shared void add(Value? meta,Value?[] supers,Value? content,Value?[]  components){
		
	}	
}

shared void testCognitivApi(){
	Engine engine=Engine();
	engine.add("MetaType",[], "MetaType", []);
	engine.add("MetaAttribute",[], "MetaAttribute", ["MetaType"]);
	engine.add("MetaRelation",[], "MetaRelation", ["MetaType", "MetaType"]);
	
	engine.add("MetaType",[], "Vehicle", []);
	engine.add("Vehicle",[], "myVehicle", []);
	engine.add("MetaType",["Vehicle"], "Car", []);
	engine.add("car",[], "myCar", []);
	engine.add("MetaType",[], "Color", []);
	engine.add("Color",[], "red", []);
	engine.add("Color",[], "blue", []);
	engine.add("Color",[], "yellow", []);
	engine.add("MetaAttribute",[], "Power", ["vehicle"]);
	engine.add ("power",[], 233, ["myVehicle"]);
	engine.add("power",[], 333, ["myCar"]);
	engine.add("MetaRelation",[], "CarColor", ["Car","Color"]);
	engine.add("CarColor",[], "myCarRed", ["myCar","red"]);
	engine.add("CarColor",[], "myCarBlue", ["myCar","blue"]);
	engine.add("MetaType",["Vehicle"], "Bike", []);
	engine.add("Bike",[], "myBike", []);
	engine.add("MetaType",[], "Robot", []);
	engine.add("MetaType",["Car","Robot"], "Transformer", []);
	engine.add("Transformer",[],"myTransformer", []);
	engine.add("MetaTree",[], "Tree", [null]);
	engine.add("Tree",[], "rootNode", [null]);
	engine.add("Tree",[], "node1", ["rootNode"]);
	engine.add("Tree",[], "node2", ["rootNode"]);
	engine.add("Tree",["node2"], "inheritingNode3", ["node2"]);
}
