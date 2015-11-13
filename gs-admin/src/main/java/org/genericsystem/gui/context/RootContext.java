package org.genericsystem.gui.context;

import javafx.beans.property.ReadOnlyObjectWrapper;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;

public class RootContext extends GenericContext {

	public RootContext(AbstractRoot engine) {
		super(null, new ReadOnlyObjectWrapper<>(engine));
	}

	@Override
	public AbstractCache getCurrentCache() {
		return (AbstractCache) genericProperty.getValue().getCurrentCache();
	}
}
