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

public abstract class Binding<U, V, T> {

	private final Binder<Function<V, T>> binder;

	public Binding(Binder<Function<V, T>> binder) {
		this.binder = binder;
	}

	public void init(ModelContext modelContext, ViewContext viewContext, Element childElement) {
		Function<V, T> initParam = buildInitParam(modelContext, viewContext);
		binder.init(initParam, modelContext, viewContext, childElement);
	}

	protected abstract Function<V, T> buildInitParam(ModelContext context, ViewContext viewContext);

	public static <U, V, T> Binding<U, V, ObservableList<T>> forEach(Function<U, ObservableList<T>> function) {
		return Binding.<U, V, ObservableList<T>> bind(function, Binder.foreachBinder());
	}

	public static <R, U, V, W> Binding<U, V, ObservableValue<W>> bindProperty(Function<R, Property<W>> getTextProperty, Function<U, ObservableValue<W>> function) {
		return Binding.<U, V, ObservableValue<W>> bind(function, Binder.propertyBinder(getTextProperty));
	}

	public static <R, U, V> Binding<U, V, Property<String>> bindInputText(Function<R, Property<String>> getTextProperty, Function<U, Property<String>> function) {
		return Binding.<U, V, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <R, U, V, T> Binding<U, V, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, Consumer<U> function) {
		return Binding.<U, V, T> bind(function, Binder.actionBinder(propAction));
	}

	public static <R, U, V, T> Binding<U, V, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, BiConsumer<U, V> function, Class<V> clazz) {
		return Binding.<U, V, T> bind(function, Binder.actionBinder(propAction));
	}

	private static <U, V, T> Binding<U, V, T> bind(Function<U, T> function, Binder<Function<V, T>> binder) {
		return new FunctionBinding<U, V, T>((u, v) -> function.apply(u), binder);
	}

	private static <U, V, T> Binding<U, V, T> bind(BiFunction<U, V, T> function, Binder<Function<V, T>> binder) {
		return new FunctionBinding<U, V, T>((u, v) -> function.apply(u, v), binder);
	}

	private static <U, V, T> FunctionBinding<U, V, T> bind(Consumer<U> function, Binder<Function<V, T>> binder) {
		return new FunctionBinding<U, V, T>((u, v) -> {
			function.accept(u);
			return null;
		}, binder);
	}

	private static <U, V, T> FunctionBinding<U, V, T> bind(BiConsumer<U, V> function, Binder<Function<V, T>> binder) {
		return new FunctionBinding<U, V, T>((u, v) -> {
			function.accept(u, v);
			return null;
		}, binder);
	}

	private static class FunctionBinding<U, V, T> extends Binding<U, V, T> {
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
					} catch (ClassCastException ignore) {
					}
					modelContext_ = modelContext_.getParent();
				}
				throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
			};
		}
	}
}
