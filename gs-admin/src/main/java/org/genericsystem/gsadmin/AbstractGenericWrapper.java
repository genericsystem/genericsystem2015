package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.utils.Transformation;

public abstract class AbstractGenericWrapper {
	protected final Generic generic;
	private final StringProperty stringProperty = new SimpleStringProperty();
	private Transformation<AbstractGenericWrapper, Generic> transformationGeneric;

	protected AbstractGenericWrapper(Generic generic) {
		this.generic = generic;
		this.stringProperty.set(this.generic.getValue().toString());
	}

	protected AbstractGenericWrapper(Generic generic, Function<Generic, ObservableList<Generic>> genericList, Function<Generic, AbstractGenericWrapper> function) {
		this(generic);
		transformationGeneric = new Transformation<AbstractGenericWrapper, Generic>(genericList.apply(generic), function);
	}

	protected ObservableValue<String> getObservableText() {
		return stringProperty;
	}

	protected ObservableList<AbstractGenericWrapper> getObservableListWrapper() {
		return transformationGeneric;
	}

}
