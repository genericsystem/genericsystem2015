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

public class Binding<M, SUBMODEL, T> {

	private final BiFunction<M, SUBMODEL, T> method;
	private final Binder<SUBMODEL, T> binder;

	public Binding(BiFunction<M, SUBMODEL, T> method, Binder<SUBMODEL, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
		Function<SUBMODEL, T> applyOnModel = applyOnModel(modelContext);
		binder.init(applyOnModel, modelContext, viewContext, childElement);
	}

	protected Function<SUBMODEL, T> applyOnModel(ModelContext modelContext) {
		return (SUBMODEL) -> {
			ModelContext modelContext_ = modelContext;
			while (modelContext_ != null) {
				try {
					return method.apply(modelContext_.getModel(), SUBMODEL);
				} catch (ClassCastException ignore) {
				}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
		};
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(Function<M, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<M, SUBMODEL, T>((u, v) -> function.apply(u), binder);
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(BiFunction<M, SUBMODEL, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<M, SUBMODEL, T>((u, v) -> function.apply(u, v), binder);
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(Consumer<M> function, Binder<SUBMODEL, T> binder) {
		return new Binding<M, SUBMODEL, T>((u, v) -> {
			function.accept(u);
			return null;
		}, binder);
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(BiConsumer<M, SUBMODEL> function, Binder<SUBMODEL, T> binder) {
		return new Binding<M, SUBMODEL, T>((u, v) -> {
			function.accept(u, v);
			return null;
		}, binder);
	}

	public static <M, SUBMODEL, T> Binding<M, SUBMODEL, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return Binding.<M, SUBMODEL, ObservableList<T>> bind(function, Binder.foreachBinder());
	}

	public static <R, M, V, W> Binding<M, V, ObservableValue<W>> bindProperty(Function<R, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.<M, V, ObservableValue<W>> bind(function, Binder.propertyBinder(getProperty));
	}

	public static <R, M, SUBMODEL> Binding<M, SUBMODEL, Property<String>> bindInputText(Function<R, Property<String>> getTextProperty, Function<M, Property<String>> function) {
		return Binding.<M, SUBMODEL, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <R, M, SUBMODEL, T> Binding<M, SUBMODEL, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, Consumer<M> function) {
		return Binding.<M, SUBMODEL, T> bind(function, Binder.actionBinder(propAction));
	}

	public static <R, M, SUBMODEL, T> Binding<M, SUBMODEL, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, BiConsumer<M, SUBMODEL> function, Class<SUBMODEL> clazz) {
		return Binding.<M, SUBMODEL, T> bind(function, Binder.actionBinder(propAction));
	}
}
