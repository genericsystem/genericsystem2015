package org.genericsystem.cache;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import org.genericsystem.kernel.Statics;

public class GsDeploymentConfig extends JsonObject {

	public GsDeploymentConfig() {
		super.put("host", Statics.DEFAULT_HOST);
		super.put("port", Statics.DEFAULT_PORT);
		super.put("engines", new JsonArray());
		super.put("classes", initClasses());

	}

	public GsDeploymentConfig setHost(String host) {
		super.put("host", host);
		return this;
	}

	public GsDeploymentConfig setPort(int port) {
		super.put("port", port);
		return this;
	}

	public GsDeploymentConfig addEngine(String engineValue, String repositoryPath) {
		super.getJsonArray("engines").add(new JsonObject().put("engineValue", engineValue).put("engineRepositoryPath", repositoryPath));
		return this;
	}

	private JsonArray initClasses() {
		JsonArray jsonArray = new JsonArray();

		jsonArray.add(new JsonObject().put("clazz", "Vehicle"));
		jsonArray.add(new JsonObject().put("clazz", "MyAudi"));
		jsonArray.add(new JsonObject().put("clazz", "MyBmw"));
		jsonArray.add(new JsonObject().put("clazz", "MyMercedes"));
		jsonArray.add(new JsonObject().put("clazz", "VehicleType"));
		jsonArray.add(new JsonObject().put("clazz", "OtherVehicleType"));
		jsonArray.add(new JsonObject().put("class", "HumanPossessVehicleTime"));
		jsonArray.add(new JsonObject().put("clazz", "ManPossessCar"));
		jsonArray.add(new JsonObject().put("clazz", "HumanPossessCar"));
		jsonArray.add(new JsonObject().put("clazz", "HumanPossessVehicle"));
		jsonArray.add(new JsonObject().put("clazz", "Time"));
		jsonArray.add(new JsonObject().put("clazz", "Myck"));
		jsonArray.add(new JsonObject().put("clazz", "Man"));
		jsonArray.add(new JsonObject().put("clazz", "Human"));
		jsonArray.add(new JsonObject().put("clazz", "Unit"));
		jsonArray.add(new JsonObject().put("clazz", "ElectrikPower"));
		jsonArray.add(new JsonObject().put("clazz", "myCar"));
		jsonArray.add(new JsonObject().put("clazz", "Car"));
		jsonArray.add(new JsonObject().put("clazz", "V123"));
		jsonArray.add(new JsonObject().put("clazz", "Voiture"));
		jsonArray.add(new JsonObject().put("clazz", "Couleur"));
		jsonArray.add(new JsonObject().put("clazz", "Puissance"));
		jsonArray.add(new JsonObject().put("clazz", "Power"));
		jsonArray.add(new JsonObject().put("clazz", "MyVehicle"));
		jsonArray.add(new JsonObject().put("clazz", "OtherVehicle"));
		jsonArray.add(new JsonObject().put("clazz", "MySelectableWindow"));
		jsonArray.add(new JsonObject().put("clazz", "SelectableWindow"));
		jsonArray.add(new JsonObject().put("clazz", "Selected"));
		jsonArray.add(new JsonObject().put("clazz", "Selectable"));
		jsonArray.add(new JsonObject().put("clazz", "Window"));
		jsonArray.add(new JsonObject().put("clazz", "Size"));
		jsonArray.add(new JsonObject().put("clazz", "GraphicComposite"));
		jsonArray.add(new JsonObject().put("clazz", "MyTransformerChildrenGames"));
		jsonArray.add(new JsonObject().put("clazz", "TransformerChildrenGames"));
		jsonArray.add(new JsonObject().put("clazz", "MyTransformer"));
		jsonArray.add(new JsonObject().put("clazz", "Transformer"));
		jsonArray.add(new JsonObject().put("clazz", "MyChildrenGames"));
		jsonArray.add(new JsonObject().put("clazz", "ChildrenGames"));
		jsonArray.add(new JsonObject().put("clazz", "MyChildren"));
		jsonArray.add(new JsonObject().put("clazz", "Children"));
		jsonArray.add(new JsonObject().put("clazz", "MyGames2"));
		jsonArray.add(new JsonObject().put("clazz", "MyGames"));
		jsonArray.add(new JsonObject().put("clazz", "Games"));

		return jsonArray;
	}
}