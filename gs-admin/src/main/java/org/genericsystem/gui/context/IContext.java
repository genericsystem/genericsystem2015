package org.genericsystem.gui.context;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;

public interface IContext {
	public IContext getParent();

	public DefaultCache<Generic> getCurrentCache();
}
