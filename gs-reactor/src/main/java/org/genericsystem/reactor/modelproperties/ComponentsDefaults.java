package org.genericsystem.reactor.modelproperties;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Context;

public interface ComponentsDefaults extends ModelProperty {

	public static final String COMPONENTS = "components";

	default void createComponentsListProperty() {
		createNewInitializedProperty(COMPONENTS, model -> new ArrayList<Property<Context>>());
	}

	default Property<List<Property<Context>>> getComponentsProperty(Context model) {
		return getProperty(COMPONENTS, model);
	}
}
