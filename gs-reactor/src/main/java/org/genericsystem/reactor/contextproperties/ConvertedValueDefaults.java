package org.genericsystem.reactor.contextproperties;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Function;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ConvertedValueDefaults extends ContextProperty {

	public static final String VALUE = "value";
	public static final String INVALID = "invalid";

	default void createConvertedValueProperty() {
		createNewContextProperty(VALUE);
	}

	default Property<Serializable> getConvertedValueProperty(Context model) {
		return getContextProperty(VALUE, model);
	}

	default <T> void initValueProperty(Function<Context, T> getInitialValue) {
		setContextPropertyValue(VALUE, getInitialValue);
	}

	default <T extends Serializable> void addConvertedValueChangeListener(BiConsumer<Context, T> listener) {
		addContextPropertyChangeListener(VALUE, listener);
	}

	default <T> void storeInvalidProperty(Function<Context, ObservableValue<T>> applyOnModel) {
		addContextAttribute(INVALID, applyOnModel);
	}

	default ObservableValue<Boolean> getInvalidObservable(Context model) {
		return getContextObservableValue(INVALID, model);
	}
}
