package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;

public class Boot {

	protected Function<Object, Property<Object>> method;

	public void init(Object node) {
		method.apply(node);
	}

	private Boot(Function<Object, Property<Object>> method) {
		this.method = method;
	}

	public static <T, S> Boot setProperty(Function<T, Property<S>> getProperty, S value) {
		return new Boot(node -> {
			getProperty.apply((T) node).setValue(value);
			return null;
		});
	}

	public static <T> Boot apply(Consumer<T> method) {
		return new Boot(object -> {
			method.accept((T) object);
			return null;
		});
	}
}
