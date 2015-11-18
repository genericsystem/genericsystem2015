package org.genericsystem.newgui.context;

public abstract class AbstractContext implements IContext {

	public IContext parent;

	public AbstractContext(IContext parent) {
		this.parent = parent;

	}

	@Override
	public IContext getParent() {
		return parent;
	}
}
