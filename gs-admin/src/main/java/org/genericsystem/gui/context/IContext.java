package org.genericsystem.gui.context;

import org.genericsystem.distributed.cacheonclient.CacheOnClient;

public interface IContext {
	public IContext getParent();

	public CacheOnClient getCurrentCache();
}
