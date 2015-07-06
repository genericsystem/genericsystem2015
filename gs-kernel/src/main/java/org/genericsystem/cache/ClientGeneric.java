package org.genericsystem.cache;

import org.genericsystem.common.TProxy;

public interface ClientGeneric extends TProxy<ClientGeneric> {

	@Override
	default ClientEngine getRoot() {
		return (ClientEngine) TProxy.super.getRoot();
	}

	@Override
	default public ClientCache getCurrentCache() {
		return (ClientCache) TProxy.super.getCurrentCache();
	}

	@Override
	default long getBirthTs() {
		return getOtherTs()[0];
	}

}
