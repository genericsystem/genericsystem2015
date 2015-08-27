package org.genericsystem.cache;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

import org.genericsystem.kernel.Statics;

public class GsDeploymentConfig extends JsonObject {

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
}