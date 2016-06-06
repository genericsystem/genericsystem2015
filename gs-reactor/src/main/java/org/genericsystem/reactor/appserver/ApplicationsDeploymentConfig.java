package org.genericsystem.reactor.appserver;

import io.vertx.core.json.JsonObject;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.EnginesDeploymentConfig.EngineDeploymentConfig;
import org.genericsystem.common.Statics;
import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.html.HtmlApp;

/**
 * @author Nicolas Feybesse
 *
 */
public class ApplicationsDeploymentConfig extends JsonObject {

	public ApplicationsDeploymentConfig() {
		this(Statics.DEFAULT_HOST, Statics.DEFAULT_PORT);
	}

	public ApplicationsDeploymentConfig(String host, int port) {
		put("apps", new JsonObject());
		put("host", host);
		put("port", port);
	}

	public String getHost() {
		return getString("host");
	}

	public int getPort() {
		return getInteger("port");
	}

	public Set<String> getApplicationsPaths() {
		return getJsonObject("apps").getMap().keySet();
	}

	public Class<? extends HtmlApp<?>> getApplicationClass(String applicationPath) {
		return getApplicationDeploymentConfig(applicationPath).getApplicationClass();
	}

	public Class<? extends Model> getModelClass(String applicationPath) {
		return getApplicationDeploymentConfig(applicationPath).getModelClass();
	}

	private ApplicationDeploymentConfig getApplicationDeploymentConfig(String applicationPath) {
		return applicationDeploymentConfig(getJsonObject("apps").getJsonObject(applicationPath));
	}

	private ApplicationDeploymentConfig applicationDeploymentConfig(JsonObject json) {
		return new ApplicationDeploymentConfig(json.getMap());
	}

	public ApplicationsDeploymentConfig addApplication(String path, Class<? extends HtmlElement<?, ?>> htmlAppClass, Class<? extends Model> modelClass, Class<? extends AbstractRoot> engineClass, String persistentDirectoryPath, Class<?>... classes) {
		getJsonObject("apps").put(path, new ApplicationDeploymentConfig(htmlAppClass, modelClass, engineClass, persistentDirectoryPath, classes));
		return this;
	}

	public void removeApplication(String path) {
		getJsonObject("apps").remove(path);
	}

	public Set<Class<?>> getClasses(String persistentDirectoryPath) {
		return getJsonObject("apps").getMap().values().stream().map(json -> applicationDeploymentConfig((JsonObject) json)).filter(conf -> Objects.equals(persistentDirectoryPath, conf.getPersistentDirectoryPath()))
				.flatMap(conf -> conf.getClasses().stream()).collect(Collectors.toSet());
	}

	public Class<? extends AbstractRoot> getEngineClass(String persistentDirectoryPath) {
		Set<Class<? extends AbstractRoot>> set = getJsonObject("apps").getMap().values().stream().map(json -> applicationDeploymentConfig((JsonObject) json)).filter(conf -> Objects.equals(persistentDirectoryPath, conf.getPersistentDirectoryPath()))
				.map(conf -> conf.getEngineClass()).collect(Collectors.toSet());
		assert set.size() == 1;
		return set.iterator().next();
	}

	public Set<String> getPersistentDirectoryPaths() {
		return getJsonObject("apps").getMap().keySet().stream().map(this::getPersistentDirectoryPath).collect(Collectors.toSet());
	}

	public String getPersistentDirectoryPath(String applicationPath) {
		return getApplicationDeploymentConfig(applicationPath).getPersistentDirectoryPath();
	}

	public static class ApplicationDeploymentConfig extends EngineDeploymentConfig {
		public ApplicationDeploymentConfig(Map<String, Object> map) {
			super(map);
			assert getString("applicationClass") != null;
		}

		public ApplicationDeploymentConfig(Class<? extends HtmlElement<?, ?>> applicationClass, Class<? extends Model> modelClass, Class<? extends AbstractRoot> engineClass, String repositoryPath, Class<?>... classes) {
			super(repositoryPath, classes);
			put("applicationClass", applicationClass.getName());
			put("modelClass", modelClass.getName());
			put("engineClass", engineClass.getName());
		}

		@SuppressWarnings("unchecked")
		public Class<? extends HtmlApp<?>> getApplicationClass() {
			try {
				return (Class<? extends HtmlApp<?>>) Class.forName(getString("applicationClass"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException(e);
			}
		}

		@SuppressWarnings("unchecked")
		public Class<? extends AbstractRoot> getEngineClass() {
			try {
				return (Class<? extends AbstractRoot>) Class.forName(getString("engineClass"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException(e);
			}
		}

		@SuppressWarnings("unchecked")
		public Class<? extends Model> getModelClass() {
			try {
				return (Class<? extends Model>) Class.forName(getString("modelClass"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException(e);
			}
		}
	}

	public static class DefaultPathSingleWebAppDeployment extends ApplicationsDeploymentConfig {

		public DefaultPathSingleWebAppDeployment(Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass, Class<? extends AbstractRoot> engineClass, Class<?>... classes) {
			addApplication("/", htmlAppClass, modelClass, engineClass, null, classes);
		}

		public DefaultPathSingleWebAppDeployment(Class<? extends AbstractRoot> applicationClass, Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass, Class<? extends AbstractRoot> engineClass, String persistentDirectoryPath,
				Class<?>... classes) {
			addApplication("/", htmlAppClass, modelClass, engineClass, persistentDirectoryPath, classes);
		}
	}

}
