package org.genericsystem.gui.context;

import org.genericsystem.distributed.cacheonclient.CacheOnClient;

public abstract class AbstractContext implements IContext {

	private final IContext parent;

	public AbstractContext(IContext parent) {
		this.parent = parent;
	}

	@Override
	public IContext getParent() {
		return this.parent;
	}

	@Override
	public CacheOnClient getCurrentCache() {
		return getParent().getCurrentCache();
	};
}
