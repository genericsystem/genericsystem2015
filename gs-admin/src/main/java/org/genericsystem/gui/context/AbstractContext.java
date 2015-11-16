package org.genericsystem.gui.context;

import org.genericsystem.common.AbstractCache;

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
	public AbstractCache getCurrentCache() {
		return getParent().getCurrentCache();
	};

}
