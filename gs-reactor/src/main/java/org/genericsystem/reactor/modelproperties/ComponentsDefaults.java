package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface ComponentsDefaults extends ContextProperty {

	public static final String COMPONENTS = "components";

	default void createComponentsListProperty() {
		createNewInitializedProperty(COMPONENTS, model -> new HashMap<Generic, Property<Serializable>>());
	}

	default Property<Map<Generic, Property<Serializable>>> getComponentsProperty(Context model) {
		return getProperty(COMPONENTS, model);
	}
}
