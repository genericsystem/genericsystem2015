package org.genericsystem.kernel;

import org.genericsystem.defaults.DefaultVertex;

public interface Generic extends DefaultVertex<Generic>, Comparable<Generic> {

	@Override
	Root getRoot();

	@Override
	default AbstractContext<Generic> getCurrentCache() {
		return (AbstractContext<Generic>) DefaultVertex.super.getCurrentCache();
	}

	default LifeManager getLifeManager() {
		return getRoot().getLifeManager(this);
	}

}
