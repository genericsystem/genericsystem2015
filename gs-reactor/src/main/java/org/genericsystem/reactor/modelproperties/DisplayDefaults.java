package org.genericsystem.reactor.modelproperties;

import javafx.beans.property.Property;

import org.genericsystem.reactor.Context;

public interface DisplayDefaults extends ContextProperty {

	public static final String DISPLAY = "display";

	default void createInitializedDisplayProperty(String initialValue) {
		createNewInitializedProperty(DISPLAY, model -> initialValue);
	}

	default Property<String> getDisplayProperty(Context model) {
		return getProperty(DISPLAY, model);
	}
}
