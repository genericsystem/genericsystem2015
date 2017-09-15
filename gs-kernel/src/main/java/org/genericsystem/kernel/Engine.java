package org.genericsystem.kernel;

import java.io.Serializable;
import java.util.Collections;
import java.util.List;

import org.genericsystem.api.core.ApiStatics;
import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractCache.ContextEventListener;
import org.genericsystem.common.Generic;
import org.genericsystem.common.GenericBuilder.SetSystemBuilder;
import org.genericsystem.common.Root;
import org.genericsystem.common.Statics;
import org.genericsystem.common.SystemCache;

/**
 * @author Nicolas Feybesse
 *
 */
public class Engine extends AbstractServer {

	public Engine(Class<?>... userClasses) {
		this(null, userClasses);
	}

	public Engine(String persistentDirectoryPath, Class<?>... userClasses) {
		init(this,
				buildHandler(getClass(), this, Collections.emptyList(), Statics.ENGINE_VALUE,
						Collections.emptyList(), ApiStatics.TS_SYSTEM, ApiStatics.SYSTEM_TS));
		startSystemCache(userClasses);
		archiver = new Archiver(this, persistentDirectoryPath);
		getCurrentCache().stop();
		isInitialized = true;
		//		newCache().start();
	}

	@Override
	protected SystemCache buildSystemCache(Root root) {
		return new SystemCache(root) {

			@Override
			protected Generic getOrBuild(AbstractCache cache, Class<?> clazz, Generic meta, List<Generic> overrides,
					Serializable value, List<Generic> components) {
				return new SetSystemBuilder(cache, clazz, meta, overrides, value, components).resolve();
			}
		};
	}

	@Override
	protected Generic init(Generic generic, DefaultHandler handler) {
		return super.init(generic, handler);
	}

	@Override
	public Cache getCurrentCache() {
		return (Cache) super.getCurrentCache();
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