package org.genericsystem.reactor.modelproperties;

import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Model;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ModelProperty<M extends Model> {

	// TODO: Delete if possible.
	<T> void storePropertyWithoutCheck(String propertyName, M model, Function<M, ObservableValue<T>> applyOnModel);

	<T> void storeProperty(String propertyName, Function<M, ObservableValue<T>> applyOnModel);

	void addPrefixBinding(Consumer<M> consumer);

	void addPostfixBinding(Consumer<M> consumer);

	<T> Property<T> getProperty(String property, Model model);

	<T> ObservableValue<T> getObservableValue(String property, Model model);

	void createNewProperty(String propertyName);

	<T> void createNewInitializedProperty(String propertyName, M model, Function<M, T> getInitialValue);
}
