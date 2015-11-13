package org.genericsystem.gui.context;

import org.genericsystem.common.AbstractCache;

public interface IContext {

	public IContext getParent();

	public AbstractCache getCurrentCache();
}
