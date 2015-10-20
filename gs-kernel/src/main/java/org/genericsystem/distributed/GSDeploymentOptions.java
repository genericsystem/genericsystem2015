package org.genericsystem.distributed;

import java.util.Map;

public class GSDeploymentOptions {

	GSDeploymentConfig config;

	public GSDeploymentOptions() {
		this.config = new GSDeploymentConfig();
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

	public GSDeploymentOptions(String engineValue, int port, String persistanceRepositoryPath) {
		this();
		config.addEngine(engineValue, persistanceRepositoryPath);
		config.setPort(port);
	}

	public GSDeploymentOptions(String engineValue, String host, int port, String persistanceRepositoryPath) {
		this();
		config.addEngine(engineValue, persistanceRepositoryPath);
		config.setPort(port);
		config.setHost(host);
	}

	public GSDeploymentOptions addEngine(String engineValue, String repositoryPath) {
		config.addEngine(engineValue, repositoryPath);
		return this;
	}

	public GSDeploymentOptions addClasses(Class<?>... classes) {
		config.addClasses(classes);
		return this;
	}

	public Map<String, String> getEngines() {
		return config.getEngines();
	}

	public Class<?>[] getClasses() {
		return config.getClasses();
	}

	public int getPort() {
		return config.getPort();
	}

	public String getHost() {
		return config.getHost();
	}
}
