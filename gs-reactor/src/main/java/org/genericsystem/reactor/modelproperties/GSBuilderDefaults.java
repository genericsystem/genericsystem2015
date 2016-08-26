package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface GSBuilderDefaults extends ModelProperty<GenericModel> {

	public static final String COMPONENTS_MAP = "componentsMap";
	public static final String HOLDERS_MAP = "holdersMap";
	public static final String INVALID_LIST = "invalidList";

	default void createHoldersMapProperty() {
		createNewInitializedProperty(HOLDERS_MAP, model -> new HashMap<Generic, Property<Serializable>>());
	}

	default void createComponentsMapProperty() {
		createNewInitializedProperty(COMPONENTS_MAP, model -> new HashMap<Generic, List<Property<GenericModel>>>());
	};

	default void createInvalidListProperty() {
		createNewInitializedProperty(INVALID_LIST, model -> new ArrayList<ObservableValue<Boolean>>());
	};

	default List<ObservableValue<Boolean>> getInvalidList(GenericModel model) {
		return this.<List<ObservableValue<Boolean>>> getProperty(INVALID_LIST, model).getValue();
	}

	default Map<Generic, Property<Serializable>> getHoldersMap(GenericModel model) {
		return this.<Map<Generic, Property<Serializable>>> getProperty(HOLDERS_MAP, model).getValue();
	}

	default Map<Generic, List<Property<GenericModel>>> getComponentsMap(GenericModel model) {
		return this.<Map<Generic, List<Property<GenericModel>>>> getProperty(COMPONENTS_MAP, model).getValue();
	}
}
