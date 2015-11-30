package org.genericsystem.todoApp.binding;

import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;

import javafx.beans.property.StringProperty;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

import org.genericsystem.todoApp.ModelContext;

public interface Binder<T> {
	public void init(T val, BindingContext context);

	public static <V, T> Binder<Consumer<V>> actionBinder() {
		return new Binder<Consumer<V>>() {
			@Override
			public void init(Consumer<V> consumer, BindingContext context) {
				context.getViewContext().setOnAction(event -> consumer.accept((V) context.getModelContext().getModel()));
			}
		};
	}

	public static <V> Binder<Function<V, StringProperty>> textBinder() {
		return new Binder<Function<V, StringProperty>>() {
			@Override
			public void init(Function<V, StringProperty> function, BindingContext context) {
				context.getViewContext().getTextProperty().bind(function.apply((V) context.getModelContext().getModel()));
			}
		};
	}

	public static <V> Binder<Function<V, StringProperty>> inputTextBinder() {
		return new Binder<Function<V, StringProperty>>() {
			@Override
			public void init(Function<V, StringProperty> function, BindingContext context) {
				context.getViewContext().getTextProperty().bindBidirectional(function.apply((V) context.getModelContext().getModel()));
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
