package org.genericsystem.reactor.modelproperties;

import java.util.ArrayList;
import java.util.List;

import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;

public interface ComponentsDefaults extends ModelProperty<GenericModel> {

	public static final String COMPONENTS = "components";

	default void createComponentsListProperty() {
		createNewInitializedProperty(COMPONENTS, model -> new ArrayList<Property<GenericModel>>());
	}

	default Property<List<Property<GenericModel>>> getComponentsProperty(GenericModel model) {
		return getProperty(COMPONENTS, model);
	}
}
