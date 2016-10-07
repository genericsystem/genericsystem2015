package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface GSBuilderDefaults extends ContextProperty {

	public static final String COMPONENTS_MAP = "componentsMap";
	public static final String HOLDERS_MAP = "holdersMap";
	public static final String INVALID_LIST = "invalidList";
	public static final String MULTIPLE_RELATION = "multipleRelation";

	default void createHoldersMapProperty() {
		createNewInitializedProperty(HOLDERS_MAP, model -> new HashMap<Generic, Property<Serializable>>());
	}

	default void createComponentsMapProperty() {
		createNewInitializedProperty(COMPONENTS_MAP, model -> new HashMap<Generic, List<Property<Context>>>());
	};

	default void createInvalidListProperty() {
		createNewInitializedProperty(INVALID_LIST, model -> new ArrayList<ObservableValue<Boolean>>());
	};

	default void createMultipleRelationProperty() {
		createNewInitializedProperty(MULTIPLE_RELATION, context -> new HashMap<Generic, Map<Generic, Property<Serializable>>>());
	}

	default Property<List<ObservableValue<Boolean>>> getInvalidListProperty(Context model) {
		return getProperty(INVALID_LIST, model);
	}

	default Property<Map<Generic, Property<Serializable>>> getHoldersMapProperty(Context model) {
		return getProperty(HOLDERS_MAP, model);
	}

	default Property<Map<Generic, List<Property<Context>>>> getComponentsMapProperty(Context model) {
		return getProperty(COMPONENTS_MAP, model);
	}

	default Property<Map<Generic, Map<Generic, Property<Serializable>>>> getMultipleRelationProperty(Context context) {
		return getProperty(MULTIPLE_RELATION, context);
	}
}
