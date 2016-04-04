package org.genericsystem.cdi;

import java.util.function.Supplier;

import javax.enterprise.inject.Vetoed;

import org.genericsystem.common.Cache;

@Vetoed
public class Engine extends org.genericsystem.kernel.Engine {

	private final Supplier<Cache> cacheSupplier;

	public Engine(Supplier<Cache> cacheSupplier, String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		assert cacheSupplier != null : "Unable to find the current cache. Did you miss to call start() method on it ?";
		this.cacheSupplier = cacheSupplier;
	}

	@Override
	public Cache getCurrentCache() {
		if (!isInitialized())
			return super.getCurrentCache();
		else {
			Cache cache = cacheSupplier.get();
			if (cache == null)
				throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
			return cache;
		}

	}
}
