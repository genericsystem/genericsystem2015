package org.genericsystem.mutability;

import org.genericsystem.defaults.DefaultVertex;

public interface Generic extends DefaultVertex<Generic>, Comparable<Generic> {

	@Override
	default Engine getRoot() {
		throw new IllegalStateException();
	}

	@Override
	default Cache getCurrentCache() {
		return getRoot().getCurrentCache();
	}
}
