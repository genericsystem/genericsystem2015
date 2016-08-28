package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

import org.genericsystem.reactor.Context;

public interface ConvertedValueDefaults extends ModelProperty {

	public static final String VALUE = "value";
	public static final String INVALID = "invalid";

	<T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener);

	default void createConvertedValueProperty() {
		createNewProperty(VALUE);
	}

	default Property<Serializable> getConvertedValueProperty(Context model) {
		return getProperty(VALUE, model);
	}

	default <T> void initValueProperty(Function<Context, T> getInitialValue) {
		initProperty(VALUE, getInitialValue);
	}

	default <T extends Serializable> void addConvertedValueChangeListener(BiConsumer<Context, T> listener) {
		addPropertyChangeListener(VALUE, listener);
	}

	default <T> void storeInvalidProperty(Function<Context, ObservableValue<T>> applyOnModel) {
		storeProperty(INVALID, applyOnModel);
	}

	default ObservableValue<Boolean> getInvalidObservable(Context model) {
		return getObservableValue(INVALID, model);
	}
}
