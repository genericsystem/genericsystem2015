package org.genericsystem.kernel;

import org.genericsystem.common.TProxy;
import org.genericsystem.kernel.Root.RootWrapped;

public interface Generic extends TProxy<Generic>, Comparable<Generic> {

	@Override
	default Root getRoot() {
		return (Root) TProxy.super.getRoot();
	}

	// @Override
	@Override
	default long getBirthTs() {
		return getLifeManager().getBirthTs();
	}

	@Override
	default public RootWrapped getProxyHandler() {
		return (RootWrapped) TProxy.super.getProxyHandler();
	}

	default LifeManager getLifeManager() {
		return getProxyHandler().getLifeManager();
	}
}
