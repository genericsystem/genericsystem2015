package org.genericsystem.reactor.contextproperties;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface ComponentsDefaults extends ContextProperty {

	public static final String COMPONENTS = "components";

	default void createComponentsMap() {
		setContextAttribute(COMPONENTS, context -> new HashMap<Generic, Property<Serializable>>());
	}

	default Map<Generic, Property<Serializable>> getComponentsMap(Context model) {
		return getContextAttribute(COMPONENTS, model);
	}
}
