package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

public interface GSBuilderDefaults extends ModelProperty {

	public static final String COMPONENTS_MAP = "componentsMap";
	public static final String HOLDERS_MAP = "holdersMap";
	public static final String INVALID_LIST = "invalidList";

	default void createHoldersMapProperty() {
		createNewInitializedProperty(HOLDERS_MAP, model -> new HashMap<Generic, Property<Serializable>>());
	}

	default void createComponentsMapProperty() {
		createNewInitializedProperty(COMPONENTS_MAP, model -> new HashMap<Generic, List<Property<Context>>>());
	};

	default void createInvalidListProperty() {
		createNewInitializedProperty(INVALID_LIST, model -> new ArrayList<ObservableValue<Boolean>>());
	};

	default List<ObservableValue<Boolean>> getInvalidList(Context model) {
		return this.<List<ObservableValue<Boolean>>> getProperty(INVALID_LIST, model).getValue();
	}

	default Map<Generic, Property<Serializable>> getHoldersMap(Context model) {
		return this.<Map<Generic, Property<Serializable>>> getProperty(HOLDERS_MAP, model).getValue();
	}

	default Map<Generic, List<Property<Context>>> getComponentsMap(Context model) {
		return this.<Map<Generic, List<Property<Context>>>> getProperty(COMPONENTS_MAP, model).getValue();
	}
}
