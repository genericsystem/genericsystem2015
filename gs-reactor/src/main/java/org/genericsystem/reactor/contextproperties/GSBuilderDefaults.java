package org.genericsystem.reactor.contextproperties;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public interface GSBuilderDefaults extends ContextProperty {

	public static final String COMPONENTS_MAP = "componentsMap";
	public static final String HOLDERS_MAP = "holdersMap";
	public static final String INVALID_LIST = "invalidList";
	public static final String MULTIPLE_RELATION = "multipleRelation";
	public static final String VALUE_COMPONENTS_MAP = "valueComponentsMap";

	public static class GenericValueComponents {
		private Property<Serializable> value = new SimpleObjectProperty<>();
		private Map<Generic, Property<Serializable>> components = new HashMap<>();

		public Property<Serializable> getGenericValue() {
			return value;
		}

		public void setGenericValue(Property<Serializable> value) {
			this.value = value;
		}

		public Map<Generic, Property<Serializable>> getComponents() {
			return components;
		}

		public void setComponents(Map<Generic, Property<Serializable>> components) {
			this.components = components;
		}

	}

	default void createValueComponentsMapProperty() {
		createNewInitializedProperty(VALUE_COMPONENTS_MAP, model -> new HashMap<Generic, GenericValueComponents>() {
			private static final long serialVersionUID = -435743147955810836L;

			@Override
			public GenericValueComponents get(Object key) {
				GenericValueComponents result = super.get(key);
				if (result == null)
					put((Generic) key, result = new GenericValueComponents());
				return result;
			};

		});
	}

	default void createHoldersMapProperty() {
		createNewInitializedProperty(HOLDERS_MAP, model -> new HashMap<Generic, Property<Serializable>>());
	}

	default void createComponentsMapProperty() {
		createNewInitializedProperty(COMPONENTS_MAP, model -> new HashMap<Generic, List<Property<Serializable>>>());
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

	default Property<Map<Generic, GenericValueComponents>> getGenericValueComponents(Context context) {
		return getProperty(VALUE_COMPONENTS_MAP, context);
	}

	default Property<Map<Generic, Property<Serializable>>> getHoldersMapProperty(Context model) {
		return getProperty(HOLDERS_MAP, model);
	}

	default Property<Map<Generic, List<Property<Serializable>>>> getComponentsMapProperty(Context model) {
		return getProperty(COMPONENTS_MAP, model);
	}

	default Property<Map<Generic, Map<Generic, Property<Serializable>>>> getMultipleRelationProperty(Context context) {
		return getProperty(MULTIPLE_RELATION, context);
	}
}
