package org.genericsystem.todoApp.binding;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

import org.genericsystem.todoApp.ModelContext;

public interface Binder<T> {
	public void init(T val, BindingContext context);

	public static <R, V, T> Binder<Consumer<V>> actionBinder(Function<R, ObjectProperty<EventHandler<ActionEvent>>> prop) {
		return new Binder<Consumer<V>>() {
			@Override
			public void init(Consumer<V> consumer, BindingContext context) {
				prop.apply((R) context.getViewContext().getNode()).set(event -> consumer.accept((V) context.getModelContext().getModel()));
			}
		};
	}

	public static <S, V> Binder<Function<V, ObservableValue<String>>> textBinder(Function<S, StringProperty> getTextProperty) {
		return new Binder<Function<V, ObservableValue<String>>>() {
			@Override
			public void init(Function<V, ObservableValue<String>> function, BindingContext context) {
				getTextProperty.apply((S) context.getViewContext().getNode()).bind(function.apply((V) context.getModelContext().getModel()));
			}
		};
	}

	public static <S, V> Binder<Function<V, StringProperty>> inputTextBinder(Function<S, StringProperty> getTextProperty) {
		return new Binder<Function<V, StringProperty>>() {
			@Override
			public void init(Function<V, StringProperty> function, BindingContext context) {
				getTextProperty.apply((S) context.getViewContext().getNode()).bindBidirectional(function.apply((V) context.getModelContext().getModel()));
			}
		};
	}

	public static <V, T> Binder<Function<V, ObservableList<T>>> foreachBinder() {

		return new Binder<Function<V, ObservableList<T>>>() {
			@SuppressWarnings("unused")
			private ListChangeListener<T> changeListener;

			@Override
			public void init(Function<V, ObservableList<T>> function, BindingContext context) {
				ObservableList<T> val = function.apply((V) context.getModelContext().getModel());
				context.getViewContext().disableInitChildren();
				Function<T, ModelContext> createChildContext = t -> context.getModelContext().createChild(t, context.getViewContext());
				List<ModelContext> children = context.getModelContext().getChildren();
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
