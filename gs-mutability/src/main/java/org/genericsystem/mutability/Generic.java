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

	@Override
	default boolean isSystem() {
		return getCurrentCache().unwrap(this).isSystem();
	}

	@Override
	default int compareTo(Generic vertex) {
		assert false;
		long birthTs = getCurrentCache().unwrap(this).getLifeManager().getBirthTs();
		long compareBirthTs = getCurrentCache().unwrap(vertex).getLifeManager().getBirthTs();
		int result = birthTs == compareBirthTs ? Long.compare(getTs(), vertex.getTs()) : Long.compare(birthTs, compareBirthTs);

		return result;
	}
}
