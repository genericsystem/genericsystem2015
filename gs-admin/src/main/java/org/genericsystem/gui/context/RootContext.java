package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CacheOnClient;
import org.genericsystem.distributed.cacheonclient.HeavyClientEngine;

public class RootContext extends AbstractContext {

	public ObjectProperty<HeavyClientEngine> rootProperty = new SimpleObjectProperty<HeavyClientEngine>();
	public ObservableList<Generic> observableGenericList;

	public RootContext(HeavyClientEngine engine) {
		super(null);
		rootProperty.set(engine);
		observableGenericList = getCurrentCache().getInstancesObservableList(rootProperty.getValue());

	}

	@Override
	public CacheOnClient getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
