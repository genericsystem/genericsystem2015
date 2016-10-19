package org.genericsystem.reactor.modelproperties;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;

public interface PasswordDefaults extends ContextProperty {

	public static final String PASSWORD_SALT = "passwordSalt";

	default void createSaltProperty() {
		createNewProperty(PASSWORD_SALT);
	}

	default Property<byte[]> getSaltProperty(Context context) {
		return getProperty(PASSWORD_SALT, context);
	}
}
