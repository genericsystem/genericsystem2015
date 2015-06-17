package org.genericsystem.cache;

import org.genericsystem.common.TProxy;

public interface Generic extends TProxy<Generic> {

	@Override
	default Engine getRoot() {
		return (Engine) TProxy.super.getRoot();
	}

	@Override
	default public Cache getCurrentCache() {
		return (Cache) TProxy.super.getCurrentCache();
	}

	@Override
	default long getBirthTs() {
		return getOtherTs()[0];
	}

}
