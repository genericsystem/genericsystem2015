package org.genericsystem.cache;

import io.vertx.core.DeploymentOptions;

public class GSDeploymentOptions extends DeploymentOptions {

	GsDeploymentConfig config = new GsDeploymentConfig();

	public GSDeploymentOptions() {
		setConfig(config);
	}

	public GSDeploymentOptions(int port) {
		this();
		config.setPort(port);
	}

	public GSDeploymentOptions(String engineValue) {
		this();
		config.addEngine(engineValue, null);
	}

	public GSDeploymentOptions(String engineValue, int port) {
		this();
		config.addEngine(engineValue, null);
		config.setPort(port);
	}

	public GSDeploymentOptions(String engineValue, int port,
			String persistanceRepositoryPath) {
		this();
		config.addEngine(engineValue, persistanceRepositoryPath);
		config.setPort(port);

	}

	public GSDeploymentOptions addEngine(String engineValue,
			String repositoryPath) {
		config.addEngine(engineValue, repositoryPath);
		return this;
	}
}
