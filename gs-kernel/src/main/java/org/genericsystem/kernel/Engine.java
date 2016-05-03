package org.genericsystem.kernel;

import java.util.Collections;
import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.Cache;
import org.genericsystem.common.Cache.ContextEventListener;
import org.genericsystem.common.Generic;

/**
 * @author Nicolas Feybesse
 *
 */
public class Engine extends AbstractServer {

	public Engine(Class<?>... userClasses) {
		this(null, userClasses);
	}

	public Engine(String persistentDirectoryPath, Class<?>... userClasses) {
		init(this, buildHandler(getClass(), (Generic) this, Collections.emptyList(), Statics.ENGINE_VALUE, Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
		startSystemCache(userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		isInitialized = true;
		newCache().start();
	}

	@Override
	protected Generic init(Generic generic, DefaultHandler handler) {
		return super.init(generic, handler);
	}

	@Override
	public Engine getRoot() {
		return this;
	}

	@Override
	public Cache newCache() {
		return new Cache(this);
	}

	public Cache newCache(ContextEventListener<Generic> listener) {
		return new Cache(this, listener);
	}
}