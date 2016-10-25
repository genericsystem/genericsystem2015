package org.genericsystem.reactor.modelproperties;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface UserRoleDefaults extends ContextProperty {

	public static final String LOGGED_USER = "loggedUser";
	public static final String CURRENT_MODE = "currentMode";

	default void createLoggedUserProperty() {
		createNewProperty(LOGGED_USER);
	}

	default void createCurrentModeProperty() {
		createNewProperty(CURRENT_MODE);
	}

	default Property<Generic> getLoggedUserProperty(Context context) {
		return getProperty(LOGGED_USER, context);
	}

	default Property<Generic> getCurrentModeProperty(Context context) {
		return getProperty(CURRENT_MODE, context);
	}
}
