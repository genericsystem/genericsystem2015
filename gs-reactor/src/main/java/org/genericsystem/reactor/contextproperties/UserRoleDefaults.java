package org.genericsystem.reactor.contextproperties;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface UserRoleDefaults extends ContextProperty {

	public static final String LOGGED_USER = "loggedUser";
	public static final String ADMIN_MODE = "adminMode";

	default void createLoggedUserProperty() {
		createNewContextProperty(LOGGED_USER);
	}

	default void createAdminModeProperty() {
		createNewContextProperty(ADMIN_MODE);
	}

	default Property<Generic> getLoggedUserProperty(Context context) {
		return getContextProperty(LOGGED_USER, context);
	}

	default Property<Boolean> getAdminModeProperty(Context context) {
		return getContextProperty(ADMIN_MODE, context);
	}
}
