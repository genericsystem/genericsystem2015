package org.genericsystem.gui.context;

import org.genericsystem.distributed.cacheonclient.CocCache;

public interface IContext {
	public IContext getParent();

	public CocCache getCurrentCache();
}
