package org.genericsystem.gui.context;

import java.util.function.Function;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;
import org.genericsystem.distributed.cacheonclient.CocClientEngine;

public class RootContext extends AbstractContext {

	public ObjectProperty<CocClientEngine> rootProperty = new SimpleObjectProperty<CocClientEngine>();
	public ObservableList<SubContext> observableSubContextList;
	private Function<Generic, SubContext> transformation;

	public RootContext(CocClientEngine engine) {
		super(null);
		rootProperty.set(engine);

		transformation = generic -> {
			SubContext subContext = new SubContext(RootContext.this);
			subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(generic);
			return subContext;
		};

		observableSubContextList = genericToSubContext(transformation, rootProperty);
	}

	@Override
	public CocCache getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
