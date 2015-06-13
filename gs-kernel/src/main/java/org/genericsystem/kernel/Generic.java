package org.genericsystem.kernel;

import java.nio.channels.IllegalSelectorException;

import javassist.util.proxy.ProxyObject;

import org.genericsystem.common.TProxy;
import org.genericsystem.common.TsDependencies;
import org.genericsystem.kernel.Root.RootWrapper;

public interface Generic extends TProxy<Generic>, Comparable<Generic> {

	@Override
	default Root getRoot() {
		throw new IllegalSelectorException();
	}

	@Override
	default long getBirthTs() {
		return ((RootWrapper) ((ProxyObject) this).getHandler()).getLifeManager().getBirthTs();
	}

	@Override
	default long getDeathTs() {
		return ((RootWrapper) ((ProxyObject) this).getHandler()).getLifeManager().getDeathTs();
	}

	default LifeManager getLifeManager() {
		return ((RootWrapper) ((ProxyObject) this).getHandler()).getLifeManager();
	}

	default TsDependencies<Generic> getDependencies() {
		return ((RootWrapper) ((ProxyObject) this).getHandler()).getDependencies();
	}

}
