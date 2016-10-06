package org.genericsystem.reactor.aa_modelproperties;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ContextProperty {

	<T> void storeProperty(String propertyName, Context model, Function<Context, ObservableValue<T>> applyOnModel);

	<T> void storeProperty(String propertyName, Function<Context, ObservableValue<T>> applyOnModel);

	void addPrefixBinding(Consumer<Context> consumer);

	void addPostfixBinding(Consumer<Context> consumer);

	<T> Property<T> getProperty(String property, Context model);

	<T> ObservableValue<T> getObservableValue(String property, Context model);

	void createNewProperty(String propertyName);

	<T> void initProperty(String propertyName, Function<Context, T> getInitialValue);

	<T> void createNewInitializedProperty(String propertyName, Function<Context, T> getInitialValue);

	<T> void createNewInitializedProperty(String propertyName, Context model, Function<Context, T> getInitialValue);

	public void addStyle(Context context, String propertyName, String value);

	<T extends Serializable> void addPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener);
}
