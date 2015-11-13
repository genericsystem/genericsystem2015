package org.genericsystem.gui.context;

import javafx.beans.property.ObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;

public class ForeachContext extends GenericContext {

	public ObservableList<GenericContext> subContexObservableList;

	public ForeachContext(IContext parent, ObjectProperty<Generic> genericProperty) {
		super(parent, genericProperty);
	}
}
