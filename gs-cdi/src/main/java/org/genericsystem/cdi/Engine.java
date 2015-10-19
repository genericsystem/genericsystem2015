package org.genericsystem.cdi;

import java.util.function.Supplier;
import javax.enterprise.inject.Vetoed;
import org.genericsystem.common.HeavyCache;

@Vetoed
public class Engine extends org.genericsystem.kernel.HeavyServerEngine {

	private final Supplier<HeavyCache> cacheSupplier;

	public Engine(Supplier<HeavyCache> cacheSupplier, String engineValue, String persistentDirectoryPath, Class<?>... userClasses) {
		super(engineValue, persistentDirectoryPath, userClasses);
		assert cacheSupplier != null : "Unable to find the current cache. Did you miss to call start() method on it ?";
		this.cacheSupplier = cacheSupplier;
		context = null;
	}

	@Override
	public HeavyCache getCurrentCache() {

		if (!isInitialized())
			return super.getCurrentCache();
		else {
			HeavyCache cache = cacheSupplier.get();
			if (cache == null)
				throw new IllegalStateException("Unable to find the current cache. Did you miss to call start() method on it ?");
			return cache;
		}

	}
}
