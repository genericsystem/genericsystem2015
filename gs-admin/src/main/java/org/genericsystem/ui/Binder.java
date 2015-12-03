package org.genericsystem.ui;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

public interface Binder<T> {
	public void init(T val, ModelContext modelContext, ViewContext viewContext, Element childElement);

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
			@SuppressWarnings("unused")
			private ListChangeListener<T> changeListener;

			@Override
			public void init(Function<V, ObservableList<T>> function, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				assert childElement != null;
				ObservableList<T> val = function.apply((V) modelContext.getModel());
				Function<T, ModelContext> createChildContext = childModel -> viewContext.createChild(childElement.classNode.isAssignableFrom(childModel.getClass()) ? childModel : (T) childElement.createNode(), modelContext, childElement);

				List<ModelContext> children = modelContext.getChildren();
				children.addAll(val.stream().map(createChildContext).collect(Collectors.toList()));
				val.addListener(new WeakListChangeListener<>(changeListener = change -> {
					while (change.next()) {
						if (change.wasPermutated()) {
							children.subList(change.getFrom(), change.getTo()).clear();
							children.addAll(change.getFrom(), change.getList().subList(change.getFrom(), change.getTo()).stream().map(createChildContext).collect(Collectors.toList()));
						} else {
							if (change.wasRemoved())
								children.subList(change.getFrom(), change.getFrom() + change.getRemovedSize()).clear();
							if (change.wasAdded())
								children.addAll(change.getFrom(), change.getAddedSubList().stream().map(createChildContext).collect(Collectors.toList()));
						}
					}
				}));
			}
		};
	}
}
