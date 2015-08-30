package org.genericsystem.kernel;

import org.genericsystem.common.GenericProxy;
import org.genericsystem.kernel.Root.RootServerHandler;

public interface Generic extends GenericProxy<Generic>, Comparable<Generic> {

	@Override
	default Root getRoot() {
		return (Root) GenericProxy.super.getRoot();
	}

	@Override
	default long getBirthTs() {
		return getLifeManager().getBirthTs();
	}

	@Override
	default public RootServerHandler getProxyHandler() {
		return (RootServerHandler) GenericProxy.super.getProxyHandler();
	}

	default LifeManager getLifeManager() {
		return getProxyHandler().getLifeManager();
	}
}
