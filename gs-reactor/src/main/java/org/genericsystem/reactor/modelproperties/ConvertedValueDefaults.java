package org.genericsystem.reactor.modelproperties;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Function;

import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ConvertedValueDefaults extends ModelProperty<GenericModel> {

	public static final String VALUE = "value";
	public static final String INVALID = "invalid";

	<T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<GenericModel, T> listener);

	default void createConvertedValueProperty() {
		createNewProperty(VALUE);
	}

	default Property<Serializable> getConvertedValueProperty(GenericModel model) {
		return getProperty(VALUE, model);
	}

	default <T> void initValueProperty(Function<GenericModel, T> getInitialValue) {
		initProperty(VALUE, getInitialValue);
	}

	default <T extends Serializable> void addConvertedValueChangeListener(BiConsumer<GenericModel, T> listener) {
		addPropertyChangeListener(VALUE, listener);
	}

	default <T> void storeInvalidProperty(Function<GenericModel, ObservableValue<T>> applyOnModel) {
		storeProperty(INVALID, applyOnModel);
	}

	default ObservableValue<Boolean> getInvalidObservable(GenericModel model) {
		return getObservableValue(INVALID, model);
	}
}
