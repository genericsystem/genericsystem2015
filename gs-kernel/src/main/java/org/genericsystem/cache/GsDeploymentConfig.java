package org.genericsystem.cache;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.kernel.Statics;

public class GsDeploymentConfig extends JsonObject {

	private Map<String, String> valueAndPath = new HashMap<>();

	public GsDeploymentConfig() {
		super.put("host", Statics.DEFAULT_HOST);
		super.put("port", Statics.DEFAULT_PORT);
		super.put("engines", new JsonArray());
		super.put("classes", new JsonArray());

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

	public GsDeploymentConfig addClasses(Class<?>... classes) {
		for (Class<?> clazz : classes)
			super.getJsonArray("classes").add(new JsonObject().put("className", clazz.getName()));
		return this;
	}

	@SuppressWarnings("unchecked")
	public Map<String, String> getEngines() {
		for (JsonObject engine : (List<JsonObject>) super.getJsonArray("engines").getList())
			valueAndPath.put(engine.getString("engineValue"), engine.getString("engineRepositoryPath"));
		return valueAndPath;
	}

	@SuppressWarnings("unchecked")
	public Class<?>[] getClasses() {
		List<JsonObject> list = super.getJsonArray("classes").getList();
		Class<?>[] classArray = new Class<?>[list.size()];
		for (int i = 0; i < list.size(); i++) {
			try {
				classArray[i] = Class.forName(list.get(i).getString("className"));
			} catch (ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return classArray;
	}

	public int getPort() {
		return super.getInteger("port");
	}

	public String getHost() {
		return super.getString("host");
	}
}