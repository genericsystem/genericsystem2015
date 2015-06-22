package org.genericsystem.kernel;

import java.io.Serializable;

import org.genericsystem.common.AbstractCache.ContextEventListener;

public class ServerEngine extends Root {

	public ServerEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public ServerEngine(Serializable engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public ServerEngine(Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		isInitialized = true;
	}

	@Override
	public ServerEngine getRoot() {
		return this;
	}

	@Override
	public ServerCache newCache() {
		return new ServerCache(this);
	}

	public ServerCache newCache(ContextEventListener<Generic> listener) {
		return new ServerCache(this, listener);
	}

	@Override
	public ServerCache getCurrentCache() {
		return (ServerCache) super.getCurrentCache();
	}
}