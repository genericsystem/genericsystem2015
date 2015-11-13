package org.genericsystem.gui.context;

import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;

public class TableViewContext extends ForeachContext {

	public ObservableValue<String> columnTitle;

	public TableViewContext(GenericContext parent) {
		super(parent, parent.genericProperty);

		Function<Generic, GenericContext> transformation = generic -> {
			GenericContext genericContext = new GenericContext(parent, genericProperty);
			genericContext.genericProperty = new ReadOnlyObjectWrapper<Generic>(generic);
			return genericContext;
		};
		Supplier<ObservableList<Generic>> observableListSupplier = () -> ((CocCache) parent.getCurrentCache()).getInstancesObservableList(genericProperty.getValue());
		columnTitle = Bindings.createStringBinding(() -> Objects.toString(genericProperty.getValue()), genericProperty);
		subContexObservableList = this.<Generic, GenericContext> getSubContextsObservableList(transformation, observableListSupplier, genericProperty);
	}
}
