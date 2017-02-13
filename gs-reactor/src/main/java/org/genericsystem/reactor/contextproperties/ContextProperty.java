package org.genericsystem.reactor.contextproperties;

import java.io.Serializable;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Context;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;

public interface ContextProperty {

	void addPrefixBinding(Consumer<Context> consumer);

	void addPostfixBinding(Consumer<Context> consumer);

	<T> T getContextAttribute(String propertyName, Context context);

	<T> Property<T> getContextProperty(String property, Context context);

	<T> ObservableValue<T> getContextObservableValue(String property, Context context);

	void createNewContextProperty(String propertyName);

	<T> void createNewInitializedProperty(String propertyName, Context context, T initialValue);

	<T> void createNewInitializedProperty(String propertyName, Function<Context, T> getInitialValue);

	<T> void addContextAttribute(String propertyName, Context context, T value);

	<T> void addContextAttribute(String propertyName, Function<Context, T> applyOnModel);

	<T> void setContextAttribute(String valueName, Context context, T value);

	<T> void setContextAttribute(String valueName, Function<Context, T> getValue);

	<T> void setContextPropertyValue(String attributeName, Context context, T value);

	<T> void setContextPropertyValue(String attributeName, Function<Context, T> getValue);

	public void addStyle(Context context, String propertyName, String value);

	<T extends Serializable> void addContextPropertyChangeListener(String propertyName, BiConsumer<Context, T> listener);
}
