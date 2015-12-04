package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;

public abstract class Boot {

	// Function<?, ?> function;
	// Object node;
	//
	// public Boot(Function<?, ?> function, Object node) {
	//
	// }
	//
	// public static <T, R> Boot setProperty(Function<T, Property<R>> getProperty, R value) {
	// return new Boot(getProperty, value) {
	//
	// };
	// }
	//
	// public static <T> Boot executeMethod(Consumer<T> method) {
	// return new Boot((m, n) -> {
	//
	// });
	// }

	public abstract void init(Object node);

	public static class BootProperty extends Boot {
		private final Function<Object, Property<Object>> getproperty;
		private final Object value;

		@SuppressWarnings({ "unchecked", "rawtypes" })
		private <T, R> BootProperty(Function<T, Property<R>> getProperty, R value) {
			this.getproperty = (Function) getProperty;
			this.value = value;
		}

		@Override
		public void init(Object node) {
			getproperty.apply(node).setValue(value);
		}

		public static <T, R> Boot setProperty(Function<T, Property<R>> getProperty, R value) {
			return new BootProperty(getProperty, value);
		}
	}

	public static class BootMethod extends Boot {

		private Consumer<Object> method;

		private BootMethod(Consumer<Object> method) {
			this.method = method;
		}

		@Override
		public void init(Object node) {
			method.accept(node);
		}

		@SuppressWarnings("unchecked")
		public static <T> Boot executeMethod(Consumer<T> method) {
			return new BootMethod((Consumer<Object>) method);
		}
	}
}
