package org.genericsystem.reactor.aa_modelproperties;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface ComponentsDefaults extends ContextProperty {

	public static final String COMPONENTS = "components";

	default void createComponentsListProperty() {
		createNewInitializedProperty(COMPONENTS, model -> new ArrayList<Property<Context>>());
	}

	default Property<List<Property<Context>>> getComponentsProperty(Context model) {
		return getProperty(COMPONENTS, model);
	}
}
