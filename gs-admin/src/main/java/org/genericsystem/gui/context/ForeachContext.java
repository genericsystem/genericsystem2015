package org.genericsystem.gui.context;

import java.util.function.Function;

import javafx.beans.property.ObjectProperty;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;

public class ForeachContext extends GenericContext {

	public ObservableList<GenericContext> subContexObservableList;

	public ForeachContext(IContext parent, ObjectProperty<Generic> genericProperty, Function<Generic, ObservableList<Generic>> originalList, Function<Generic, GenericContext> extractor) {
		super(parent, genericProperty);
		this.subContexObservableList = IContext.getSubContextsObservableList(originalList.apply(genericProperty.get()), extractor, genericProperty);
	}
}
