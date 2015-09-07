package org.genericsystem.kernel;

import org.genericsystem.common.Cache;
import org.genericsystem.common.Cache.ContextEventListener;
import org.genericsystem.common.IDifferential;

public class ServerEngine extends Root {

	public ServerEngine(Class<?>... userClasses) {
		this(Statics.ENGINE_VALUE, userClasses);
	}

	public ServerEngine(String engineValue, Class<?>... userClasses) {
		this(engineValue, null, userClasses);
	}

	public ServerEngine(String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
	}

	@Override
	public ServerEngine getRoot() {
		return this;
	}

	@Override
	public Cache newCache() {
		return new Cache(this) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((Root) getRoot());
			}
		};
	}

	public Cache newCache(ContextEventListener<Generic> listener) {
		return new Cache(this, listener) {
			@Override
			protected IDifferential<Generic> buildTransaction() {
				return new Transaction((Root) getRoot());
			}
		};
	}
}