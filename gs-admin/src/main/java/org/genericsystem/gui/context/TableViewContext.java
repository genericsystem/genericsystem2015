package org.genericsystem.gui.context;

import java.util.Objects;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.cacheonclient.CocCache;

public class TableViewContext extends ForeachContext {

	public ObservableValue<String> columnTitle;

	public TableViewContext(GenericContext parent) {
		super(parent, parent.genericProperty, generic -> ((CocCache) parent.getCurrentCache()).getInstancesObservableList(generic), generic -> new GenericContext(parent, new ReadOnlyObjectWrapper<Generic>(generic)));
		columnTitle = Bindings.createStringBinding(() -> Objects.toString(genericProperty.getValue()), genericProperty);
	}
}
