package org.genericsystem.common;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;

public class EnginesDeploymentConfig extends JsonObject {

	public EnginesDeploymentConfig() {
		this(Statics.DEFAULT_HOST, Statics.ENGINES_DEFAULT_PORT);
	}

	public EnginesDeploymentConfig(String host, int port) {
		put("engines", new JsonObject());
		put("host", host);
		put("port", port);
	}

	public EnginesDeploymentConfig addEngine(String path, String persistentDirectoryPath, Class<?>... classes) {
		if (getJsonObject("engines").getJsonObject(path) != null)
			throw new IllegalStateException("Path already exists : " + path);
		getJsonObject("engines").put(path, new EngineDeploymentConfig(persistentDirectoryPath, classes));
		return this;
	}

	public Set<String> getEnginePaths() {
		return getJsonObject("engines").getMap().keySet();
	}

	public String getHost() {
		return getString("host");
	}

	public int getPort() {
		return getInteger("port");
	}

	@Override
	public String toString() {
		return encodePrettily();
	}

	public List<Class<?>> getClasses(String path) {
		return new EngineDeploymentConfig(getJsonObject("engines").getJsonObject(path).getMap()).getClasses();
	}

	public String getPersistentDirectoryPath(String path) {
		return new EngineDeploymentConfig(getJsonObject("engines").getJsonObject(path).getMap()).getPersistentDirectoryPath();
	}

	public static class EngineDeploymentConfig extends JsonObject {
		protected EngineDeploymentConfig(Map<String, Object> map) {
			super(map);
		}

		public EngineDeploymentConfig(String persistentDirectoryPath, Class<?>... classes) {
			super.put("persistenceDirectoryPath", persistentDirectoryPath);
			super.put("classes", new JsonArray());
			addClasses(classes);
		}

		public EngineDeploymentConfig addClasses(Class<?>... classes) {
			for (Class<?> clazz : classes)
				super.getJsonArray("classes").add(clazz.getName());
			return this;
		}

		@SuppressWarnings("unchecked")
		public List<Class<?>> getClasses() {
			return (List<Class<?>>) getJsonArray("classes").getList().stream().map(className -> {
				try {
					return Class.forName((String) className);
				} catch (ClassNotFoundException e) {
					throw new IllegalStateException(e);
				}
			}).collect(Collectors.toList());
		}

		public String getPersistentDirectoryPath() {
			return super.getString("persistenceDirectoryPath");
		}
	}

	public static class DefaultPathSingleEngineDeployment extends EnginesDeploymentConfig {
		public DefaultPathSingleEngineDeployment(Class<?>... classes) {
			this(null, classes);
		}

		public DefaultPathSingleEngineDeployment(String persistentDirectoryPath, Class<?>... classes) {
			addEngine("/", persistentDirectoryPath, classes);
		}
	}
}
