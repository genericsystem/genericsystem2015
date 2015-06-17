package org.genericsystem.kernel;

import org.genericsystem.common.TProxy;
import org.genericsystem.common.TsDependencies;
import org.genericsystem.kernel.Root.RootWrapper;

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

	// @Override
	// default long getDeathTs() {
	// return ((RootWrapper) ((ProxyObject) this).getHandler()).getLifeManager().getDeathTs();
	// }

	@Override
	default public RootWrapper getRootWrapper() {
		return (RootWrapper) TProxy.super.getRootWrapper();
	}

	default LifeManager getLifeManager() {
		return getRootWrapper().getLifeManager();
	}

	default TsDependencies<Generic> getDependencies() {
		return getRootWrapper().getDependencies();
	}

}
