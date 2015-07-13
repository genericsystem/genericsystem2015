package org.genericsystem.cache;

import org.genericsystem.common.GenericProxy;

public interface ClientGeneric extends GenericProxy<ClientGeneric> {

	@Override
	default ClientEngine getRoot() {
		return (ClientEngine) GenericProxy.super.getRoot();
	}

	@Override
	default public ClientCache getCurrentCache() {
		return (ClientCache) GenericProxy.super.getCurrentCache();
	}

	@Override
	default long getBirthTs() {
		return getOtherTs()[0];
	}

}
