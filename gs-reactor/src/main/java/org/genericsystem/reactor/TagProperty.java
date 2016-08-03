package org.genericsystem.reactor;

import java.io.Serializable;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.util.StringConverter;

public abstract class TagProperty<T> {

	private final Map<Generic, ObservableValue<T>> observables;

	private TagProperty() {
		observables = new HashMap<>();
	}

	protected void setObservable(Generic generic, ObservableValue<T> observable) {
		observables.put(generic, observable);
	}

	public ObservableValue<T> getObservable(Generic generic) {
		return observables.get(generic);
	}

	public Property<T> getProperty(Generic generic) {
		return (Property<T>) observables.get(generic);
	}

	public T getValue(Generic generic) {
		return observables.get(generic).getValue();
	}

	public void setValue(Generic generic, T value) {
		((Property<T>) observables.get(generic)).setValue(value);
	}

	public static class CheckedProperty extends TagProperty<Boolean> {

	}

	public static class CompletedProperty extends TagProperty<Boolean> {

	}

	public static class ComponentsProperty extends TagProperty<List<Property<GenericModel>>> {

	}

	public static class ConverterProperty<S extends Serializable> extends TagProperty<StringConverter<S>> {

	}

	public static class DisabledProperty extends TagProperty<String> {

	}

	public static class DisplayProperty extends TagProperty<String> {

	}

	public static class SelectionProperty extends TagProperty<GenericModel> {

	}

	public static class SelectionIndexProperty extends TagProperty<Number> {

	}

	public static class TextProperty extends TagProperty<String> {

	}

	public static class ValueProperty<S extends Serializable> extends TagProperty<S> {

	}

	public static class SelectionShiftProperty extends TagProperty<Integer> {

	}

	public static class InvalidProperty extends TagProperty<Boolean> {

	}

	public static class SelectionStringProperty extends TagProperty<String> {

	}

	public static class HasNoTodoProperty extends TagProperty<Boolean> {

	}

	public static class AllModeProperty extends TagProperty<Boolean> {

	}

	public static class ActiveModeProperty extends TagProperty<Boolean> {

	}

	public static class HasNoCompletedProperty extends TagProperty<Boolean> {

	}

	public static class CompleteModeProperty extends TagProperty<Boolean> {

	}
}