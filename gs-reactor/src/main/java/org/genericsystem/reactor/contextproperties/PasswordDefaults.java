package org.genericsystem.reactor.contextproperties;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface PasswordDefaults extends ContextProperty {

	public static final String PASSWORD_SALT = "passwordSalt";

	default void createSaltProperty() {
		createNewContextProperty(PASSWORD_SALT);
	}

	default Property<byte[]> getSaltProperty(Context context) {
		return getContextProperty(PASSWORD_SALT, context);
	}
}
