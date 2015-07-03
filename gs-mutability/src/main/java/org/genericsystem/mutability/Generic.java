package org.genericsystem.mutability;

import java.io.Serializable;
import java.util.List;

import org.genericsystem.defaults.DefaultVertex;

public interface Generic extends DefaultVertex<Generic> {

	@Override
	default Engine getRoot() {
		throw new IllegalStateException();
	}

	@Override
	default Cache getCurrentCache() {
		return getRoot().getCurrentCache();
	}

	@Override
	default long getTs() {
		return getCurrentCache().unwrap(this).getBirthTs();
	}

	@Override
	default Generic getMeta() {
		return getCurrentCache().wrap(getCurrentCache().unwrap(this).getMeta());
	}

	@Override
	default List<Generic> getSupers() {
		return getCurrentCache().wrap(getCurrentCache().unwrap(this).getSupers());
	}

	@Override
	default List<Generic> getComponents() {
		return getCurrentCache().wrap(getCurrentCache().unwrap(this).getComponents());
	}

	@Override
	default Serializable getValue() {
		return getCurrentCache().unwrap(this).getValue();
	}

	@Override
	default long getBirthTs() {
		return getCurrentCache().unwrap(this).getBirthTs();
	}

	@Override
	default long[] getOtherTs() {
		return getCurrentCache().unwrap(this).getOtherTs();
	}
}
