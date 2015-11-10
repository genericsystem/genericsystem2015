package org.genericsystem.gui.context;

import org.genericsystem.distributed.cacheonclient.CocCache;

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
	public CocCache getCurrentCache() {
		return getParent().getCurrentCache();
	};
}
