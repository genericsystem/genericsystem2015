package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;

public class Boot<NODE> {

	private final Function<NODE, Property<?>> method;

	private Boot(Function<NODE, Property<?>> method) {
		this.method = method;
	}

	public void init(NODE node) {
		method.apply(node);
	}

	public static <NODE, VALUE> Boot<NODE> setProperty(Function<NODE, Property<VALUE>> getProperty, VALUE value) {
		return new Boot<>(node -> {
			getProperty.apply(node).setValue(value);
			return null;
		});
	}

	public static <NODE> Boot<NODE> apply(Consumer<NODE> method) {
		return new Boot<>(object -> {
			method.accept(object);
			return null;
		});
	}
}
