package org.genericsystem.gui.context;

import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.DefaultCache;

public class RootContext extends AbstractContext {

	public ObjectProperty<Generic> rootProperty = new SimpleObjectProperty<Generic>();
	// public ObjectProperty<CocClientEngine> rootProperty = new SimpleObjectProperty<CocClientEngine>();
	public ObservableList<SubContext> observableSubContextList;
	public ObservableValue<String> columnTitle;

	private Function<Generic, SubContext> transformation;

	public RootContext(Generic engine) {
		super(null);
		rootProperty.set(engine);

		columnTitle = Bindings.createStringBinding(() -> rootProperty.getValue().toString(), rootProperty);

		transformation = generic -> {
			SubContext subContext = new SubContext(RootContext.this);
			subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(generic);
			return subContext;
		};
		observableSubContextList = genericToSubContext(transformation, rootProperty);
	}

	@Override
	public DefaultCache<Generic> getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
