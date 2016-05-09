package org.genericsystem.cdi;

import java.util.function.Supplier;

import javax.enterprise.inject.Vetoed;

import org.genericsystem.common.AbstractCache;

@Vetoed
public class Engine extends org.genericsystem.kernel.Engine {

	private final Supplier<AbstractCache> cacheSupplier;

	public Engine(Supplier<AbstractCache> cacheSupplier, String persistentDirectoryPath, Class<?>... userClasses) {
		super(persistentDirectoryPath, userClasses);
		assert cacheSupplier != null : "Unable to find the current cache. Did you miss to call start() method on it ?";
		this.cacheSupplier = cacheSupplier;
	}

	@Override
	public AbstractCache getCurrentCache() {
		if (!isInitialized())
			return super.getCurrentCache();
		else {
			AbstractCache cache = cacheSupplier.get();
			if (cache == null)
				throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
			return cache;
		}

	}
}
