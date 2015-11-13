package org.genericsystem.gui.context;

import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.AbstractCache;
import org.genericsystem.common.AbstractRoot;
import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;

public class RootContext extends AbstractContext {

	public ObjectProperty<AbstractRoot> rootProperty = new SimpleObjectProperty<>();
	public ObservableList<SubContext> subContexObservableList;
	public ObservableValue<String> columnTitle;

	public RootContext(AbstractRoot engine) {
		super(null);
		rootProperty.set(engine);

		columnTitle = Bindings.createStringBinding(() -> rootProperty.getValue().toString(), rootProperty);

		Function<Generic, SubContext> transformation = generic -> {
			SubContext subContext = new SubContext(RootContext.this);
			subContext.observableGeneric = new ReadOnlyObjectWrapper<Generic>(generic);
			return subContext;
		};

		Supplier<ObservableList<Generic>> observableListSupplier = () -> ((CocCache) getCurrentCache()).getInstancesObservableList(engine);

		subContexObservableList = this.<Generic, SubContext> getSubContextsObservableList(transformation, observableListSupplier, rootProperty);
	}

	@Override
	public AbstractCache getCurrentCache() {
		return rootProperty.getValue().getCurrentCache();
	}

}
