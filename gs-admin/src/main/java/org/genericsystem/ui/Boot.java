package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.Property;

public class Boot<NODE> {

	private final Consumer<NODE> consumer;

	private Boot(Consumer<NODE> consumer) {
		this.consumer = consumer;
	}

	public void init(NODE node) {
		consumer.accept(node);
	}

	public static <NODE, VALUE> Boot<NODE> setProperty(Function<NODE, Property<VALUE>> getProperty, VALUE value) {
		return new Boot<>(node -> getProperty.apply(node).setValue(value));
	}

	public static <NODE> Boot<NODE> apply(Consumer<NODE> method) {
		return new Boot<>(object -> method.accept(object));
	}
}
