package org.genericsystem.kernel;

import org.genericsystem.common.GenericProxy;
import org.genericsystem.kernel.Root.RootWrapped;

public interface Generic extends GenericProxy<Generic>, Comparable<Generic> {

	@Override
	default Root getRoot() {
		return (Root) GenericProxy.super.getRoot();
	}

	// @Override
	@Override
	default long getBirthTs() {
		return getLifeManager().getBirthTs();
	}

	@Override
	default public RootWrapped getProxyHandler() {
		return (RootWrapped) GenericProxy.super.getProxyHandler();
	}

	default LifeManager getLifeManager() {
		return getProxyHandler().getLifeManager();
	}
}
