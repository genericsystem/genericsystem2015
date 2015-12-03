package org.genericsystem.ui;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

public interface Binder<T> {
	public void init(T val, ModelContext modelContext, ViewContext viewContext, Element childElement);

	public static <T> Binder<Property<T>> setValueProperty(T value) {
		return new Binder<Property<T>>() {
			@Override
			public void init(Property<T> property, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				property.setValue(value);
			}

		};
	}

	public static <T> Binder<Consumer<T>> methodBinder() {
		return new Binder<Consumer<T>>() {
			@Override
			public void init(Consumer<T> val, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				val.accept((T) viewContext.getNode());
			}
		};
	}

	public static <R, V, T> Binder<Consumer<V>> actionBinder(Function<R, ObjectProperty<EventHandler<ActionEvent>>> prop) {
		return new Binder<Consumer<V>>() {
			@Override
			public void init(Consumer<V> consumer, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				prop.apply((R) viewContext.getNode()).set(event -> consumer.accept((V) modelContext.getModel()));
			}
		};
	}

	public static <S, V, W> Binder<Function<V, ObservableValue<W>>> propertyBinder(Function<S, Property<W>> getProperty) {
		return new Binder<Function<V, ObservableValue<W>>>() {
			@Override
			public void init(Function<V, ObservableValue<W>> function, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				getProperty.apply((S) viewContext.getNode()).bind(function.apply((V) modelContext.getModel()));
			}
		};
	}

	public static <S, V> Binder<Function<V, Property<String>>> inputTextBinder(Function<S, Property<String>> getTextProperty) {
		return new Binder<Function<V, Property<String>>>() {
			@Override
			public void init(Function<V, Property<String>> function, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				getTextProperty.apply((S) viewContext.getNode()).bindBidirectional(function.apply((V) modelContext.getModel()));
			}
		};
	}

	public static <V, T> Binder<Function<V, ObservableList<T>>> foreachBinder() {

		return new Binder<Function<V, ObservableList<T>>>() {

			private List<T> list;

			@Override
			public void init(Function<V, ObservableList<T>> function, ModelContext modelContext, ViewContext viewContext, Element childElement) {

				list = new AbstractList<T>() {

					@Override
					public T get(int index) {
						return (T) modelContext.get(index).getModel();
					}

					@Override
					public int size() {
						return modelContext.size();
					}

					@Override
					public void add(int index, T element) {
						modelContext.createSubContext(viewContext, index, element, childElement);
					}

					@Override
					public T set(int index, T element) {
						T remove = remove(index);
						add(index, element);
						return remove;
					}

					@Override
					public T remove(int index) {
						return (T) modelContext.removeSubContext(index).getModel();
					}

				};
				Bindings.bindContent(list, function.apply((V) modelContext.getModel()));
			}
		};
	}
}
