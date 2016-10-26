package org.genericsystem.reactor.modelproperties;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface UserRoleDefaults extends ContextProperty {

	public static final String LOGGED_USER = "loggedUser";
	public static final String ADMIN_MODE = "adminMode";

	default void createLoggedUserProperty() {
		createNewProperty(LOGGED_USER);
	}

	default void createAdminModeProperty() {
		createNewProperty(ADMIN_MODE);
	}

	default Property<Generic> getLoggedUserProperty(Context context) {
		return getProperty(LOGGED_USER, context);
	}

	default Property<Boolean> getAdminModeProperty(Context context) {
		return getProperty(ADMIN_MODE, context);
	}
}
