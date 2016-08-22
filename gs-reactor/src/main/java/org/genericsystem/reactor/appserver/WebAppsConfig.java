package org.genericsystem.reactor.appserver;

import io.vertx.core.json.JsonObject;

import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.genericsystem.common.EnginesDeploymentConfig.EngineDeploymentConfig;
import org.genericsystem.common.Root;
import org.genericsystem.common.Statics;
import org.genericsystem.kernel.Engine;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Tag.RootTag;
import org.genericsystem.reactor.annotations.DependsOnModel;
import org.genericsystem.reactor.gs.GSApp;
import org.genericsystem.reactor.html.HtmlApp;
import org.genericsystem.reactor.model.RootModel;

/**
 * @author Nicolas Feybesse
 *
 */
public class WebAppsConfig extends JsonObject {

	public WebAppsConfig() {
		this(Statics.DEFAULT_PORT);
	}

	public WebAppsConfig(String[] mainArgs) {
		this(mainArgs.length != 0 ? Integer.parseInt(mainArgs[0]) : Statics.DEFAULT_PORT);
	}

	public WebAppsConfig(int port) {
		put("apps", new JsonObject());
		put("host", Statics.DEFAULT_HOST);
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

	public Class<? extends RootTag<?>> getApplicationClass(String applicationPath) {
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

	public static Class<?>[] getModelClasses(Class<?> applicationClass) {
		DependsOnModel dependOn = applicationClass.getAnnotation(DependsOnModel.class);
		return dependOn != null ? dependOn.value() : new Class[] {};
	}

	public WebAppsConfig addApplication(String path, Class<? extends Tag<?>> htmlAppClass, Class<? extends Model> modelClass, Class<? extends Root> engineClass, String persistentDirectoryPath) {
		getJsonObject("apps").put(path, new ApplicationDeploymentConfig(htmlAppClass, modelClass, engineClass, persistentDirectoryPath, getModelClasses(htmlAppClass)));
		return this;
	}

	public void removeApplication(String path) {
		getJsonObject("apps").remove(path);
	}

	public Set<Class<?>> getClasses(String persistentDirectoryPath) {
		return getJsonObject("apps").getMap().values().stream().map(json -> applicationDeploymentConfig((JsonObject) json)).filter(conf -> Objects.equals(persistentDirectoryPath, conf.getPersistentDirectoryPath()))
				.flatMap(conf -> conf.getClasses().stream()).collect(Collectors.toSet());
	}

	public Class<? extends Root> getEngineClass(String persistentDirectoryPath) {
		Set<Class<? extends Root>> set = getJsonObject("apps").getMap().values().stream().map(json -> applicationDeploymentConfig((JsonObject) json)).filter(conf -> Objects.equals(persistentDirectoryPath, conf.getPersistentDirectoryPath()))
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

		public ApplicationDeploymentConfig(Class<? extends Tag<?>> applicationClass, Class<? extends Model> modelClass, Class<? extends Root> engineClass, String repositoryPath, Class<?>... classes) {
			super(repositoryPath, classes);
			put("applicationClass", applicationClass.getName());
			put("modelClass", modelClass.getName());
			put("engineClass", engineClass.getName());
		}

		@SuppressWarnings("unchecked")
		public Class<? extends RootTag<?>> getApplicationClass() {
			try {
				return (Class<? extends RootTag<?>>) Class.forName(getString("applicationClass"));
			} catch (ClassNotFoundException e) {
				throw new IllegalStateException(e);
			}
		}

		@SuppressWarnings("unchecked")
		public Class<? extends Root> getEngineClass() {
			try {
				return (Class<? extends Root>) Class.forName(getString("engineClass"));
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

	public static class SimpleWebAppConfig extends WebAppsConfig {
		public SimpleWebAppConfig(String[] mainArgs, Class<? extends GSApp> htmlAppClass, String homePersistentDirectoryPath) {
			super(mainArgs);
			addApplication("/", htmlAppClass, RootModel.class, Engine.class, System.getenv("HOME") + "/genericsystem/" + homePersistentDirectoryPath);
		}

		public SimpleWebAppConfig(String[] mainArgs, Class<? extends HtmlApp<?>> htmlAppClass, Class<? extends Model> modelClass, String homePersistentDirectoryPath) {
			super(mainArgs);
			addApplication("/", htmlAppClass, modelClass, Engine.class, System.getenv("HOME") + "/genericsystem/" + homePersistentDirectoryPath);
		}
	}

	public String getRootId() {
		return "root";
	}

}
