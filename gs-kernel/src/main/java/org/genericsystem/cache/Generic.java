package org.genericsystem.cache;

import java.nio.channels.IllegalSelectorException;

import org.genericsystem.common.TProxy;

public interface Generic extends TProxy<Generic> {

	@Override
	default Engine getRoot() {
		throw new IllegalSelectorException();
	}

	@Override
	default public Cache getCurrentCache() {
		return (Cache) TProxy.super.getCurrentCache();
	}

	@Override
	default long getBirthTs() {
		return getVertex().getOtherTs()[0];
	}
}
