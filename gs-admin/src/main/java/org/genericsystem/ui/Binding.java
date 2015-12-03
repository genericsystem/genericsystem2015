package org.genericsystem.ui;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

public abstract class Binding<B> {

	private final Binder<B> binder;

	public Binding(Binder<B> binder) {
		this.binder = binder;
	}

	public void init(ModelContext modelContext, ViewContext viewContext, Element childElement) {
		B initParam = buildInitParam(modelContext, viewContext);
		binder.init(initParam, modelContext, viewContext, childElement);
	}

	protected abstract B buildInitParam(ModelContext context, ViewContext viewContext);

	public static <U, V, T> Binding<Function<V, ObservableList<T>>> forEach(Function<U, ObservableList<T>> function) {
		return Binding.<U, V, ObservableList<T>> bind(function, Binder.foreachBinder());
	}

	public static <R, U, V, W> FunctionBinding<U, V, ObservableValue<W>> bindProperty(Function<R, Property<W>> getTextProperty, Function<U, ObservableValue<W>> function) {
		return Binding.<U, V, ObservableValue<W>> bind(function, Binder.propertyBinder(getTextProperty));
	}

	public static <R, U, V> FunctionBinding<U, V, Property<String>> bindInputText(Function<R, Property<String>> getTextProperty, Function<U, Property<String>> function) {
		return Binding.<U, V, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <R, U, V, T> ConsumerBinding<U, V, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, Consumer<U> function) {
		return Binding.<U, V, T> bind(function, Binder.actionBinder(propAction));
	}

	public static <R, U, V, T> ConsumerBinding<U, V, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, BiConsumer<U, V> function, Class<V> clazz) {
		return Binding.<U, V, T> bind(function, clazz, Binder.actionBinder(propAction));
	}

	private static <U, V, T> FunctionBinding<U, V, T> bind(Function<U, T> function, Binder<Function<V, T>> binder) {
		return new FunctionBinding<U, V, T>((u, v) -> function.apply(u), binder);
	}

	// private static <U, V, T> FunctionBinding<U, V, T> bind(BiFunction<U, V, T> function, Binder<Function<V, T>> binder) {
	// return new FunctionBinding<U, V, T>((u, v) -> function.apply(u, v), binder);
	// }

	private static <U, V, T> ConsumerBinding<U, V, T> bind(Consumer<U> function, Binder<Consumer<V>> binder) {
		return new ConsumerBinding<U, V, T>((u, v) -> {
			function.accept(u);
			return null;
		}, binder);
	}

	private static <U, V, T> ConsumerBinding<U, V, T> bind(BiConsumer<U, V> function, Class<V> clazz2, Binder<Consumer<V>> binder) {
		return new ConsumerBinding<U, V, T>((u, v) -> {
			function.accept(u, v);
			return null;
		}, binder);
	}

	private static class FunctionBinding<U, V, T> extends Binding<Function<V, T>> {
		private final BiFunction<U, V, T> method;

		public FunctionBinding(BiFunction<U, V, T> method, Binder<Function<V, T>> binder) {
			super(binder);
			this.method = method;
		}

		@Override
		protected Function<V, T> buildInitParam(ModelContext modelContext, ViewContext viewContext) {
			return (v) -> {
				ModelContext modelContext_ = modelContext;
				while (modelContext_ != null) {
					try {
						return method.apply((U) modelContext_.getModel(), v);
					} catch (ClassCastException ignore) {}
					modelContext_ = modelContext_.getParent();
				}
				throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
			};
		}
	}

	private static class ConsumerBinding<U, V, T> extends Binding<Consumer<V>> {
		private final BiFunction<U, V, T> method;

		public ConsumerBinding(BiFunction<U, V, T> method, Binder<Consumer<V>> binder) {
			super(binder);
			this.method = method;
		}

		@Override
		protected Consumer<V> buildInitParam(ModelContext modelContext, ViewContext viewContext) {
			return (v) -> {
				ModelContext modelContext_ = modelContext;
				while (modelContext_ != null) {
					try {
						method.apply((U) modelContext_.getModel(), v);
						return;
					} catch (ClassCastException ignore) {}
					modelContext_ = modelContext_.getParent();
				}
				throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
			};
		}
	}

}
