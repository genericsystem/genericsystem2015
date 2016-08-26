package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;

public interface DisplayDefaults extends ModelProperty<GenericModel> {

	public static final String DISPLAY = "display";

	default void createInitializedDisplayProperty(String initialValue) {
		createNewInitializedProperty(DISPLAY, model -> initialValue);
	}

	default Property<String> getDisplayProperty(GenericModel model) {
		return getProperty(DISPLAY, model);
	}
}
