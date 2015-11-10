package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

public class RootContext extends AbstractContext {

	public ObjectProperty<CocClientEngine> rootProperty = new SimpleObjectProperty<CocClientEngine>();
	public ObservableList<Generic> observableGenericList;

	public RootContext(CocClientEngine engine) {
		super(null);
		rootProperty.set(engine);
		observableGenericList = getCurrentCache().getInstancesObservableList(rootProperty.getValue());

	}

	@Override
	public CocCache getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
