package org.genericsystem.cdi;

import java.io.Serializable;
import java.util.function.Supplier;

import javax.enterprise.inject.Vetoed;

import org.genericsystem.mutability.Cache;

@Vetoed
public class Engine extends org.genericsystem.mutability.Engine {

	private final Supplier<Cache> cacheSupplier;

	public Engine(Supplier<Cache> cacheSupplier, Serializable engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		assert cacheSupplier != null : "Unable to find the current cache. Did you miss to call start() method on it ?";
		this.cacheSupplier = cacheSupplier;
		getCurrentCache().stop();
	}

	@Override
	public Cache getCurrentCache() {
		Cache cacheInThreadLocal = cacheLocal.get();
		if (cacheInThreadLocal != null)
			return cacheInThreadLocal;
		Cache cache = cacheSupplier.get();
		if (cache == null)
			throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
		return cache;
	}

}
