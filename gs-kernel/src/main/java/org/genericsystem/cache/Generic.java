package org.genericsystem.cache;

import java.nio.channels.IllegalSelectorException;

import org.genericsystem.common.TProxy;

public interface Generic extends TProxy<Generic> {

	@Override
	default Engine getRoot() {
		throw new IllegalSelectorException();
	}

	@Override
	default long getBirthTs() {
		return getVertex().getOtherTs()[0];
	}

	@Override
	default long getDeathTs() {
		return getVertex().getOtherTs()[2];
	}
}
