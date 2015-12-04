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

public class Binding<MODEL, SUBMODEL, T> {

	private final BiFunction<?, SUBMODEL, T> method;
	private final Binder<SUBMODEL, T> binder;

	public Binding(BiFunction<MODEL, SUBMODEL, T> method, Binder<SUBMODEL, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<?> viewContext, Element<?> childElement) {
		Function<SUBMODEL, T> applyOnModel = applyOnModel(modelContext);
		binder.init(applyOnModel, modelContext, viewContext, childElement);
	}

	protected Function<SUBMODEL, T> applyOnModel(ModelContext modelContext) {
		return (SUBMODEL) -> {
			ModelContext modelContext_ = modelContext;
			while (modelContext_ != null) {
				try {
					return method.apply(modelContext_.getModel(), SUBMODEL);
				} catch (ClassCastException ignore) {}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
		};
	}

	private static <MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bind(Function<MODEL, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<MODEL, SUBMODEL, T>((u, v) -> function.apply(u), binder);
	}

	private static <MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bind(BiFunction<MODEL, SUBMODEL, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<MODEL, SUBMODEL, T>((u, v) -> function.apply(u, v), binder);
	}

	private static <MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bind(Consumer<MODEL> function, Binder<SUBMODEL, T> binder) {
		return new Binding<MODEL, SUBMODEL, T>((u, v) -> {
			function.accept(u);
			return null;
		}, binder);
	}

	private static <MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bind(BiConsumer<MODEL, SUBMODEL> function, Binder<SUBMODEL, T> binder) {
		return new Binding<MODEL, SUBMODEL, T>((u, v) -> {
			function.accept(u, v);
			return null;
		}, binder);
	}

	public static <MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, ObservableList<T>> forEach(Function<MODEL, ObservableList<T>> function) {
		return Binding.<MODEL, SUBMODEL, ObservableList<T>> bind(function, Binder.foreachBinder());
	}

	public static <R, MODEL, V, W> Binding<MODEL, V, ObservableValue<W>> bindProperty(Function<R, Property<W>> getProperty, Function<MODEL, ObservableValue<W>> function) {
		return Binding.<MODEL, V, ObservableValue<W>> bind(function, Binder.propertyBinder(getProperty));
	}

	// public static <R, MODEL, V, W> Binding<MODEL, V, ObservableValue<W>> setProperty(Function<R, Property<W>> getTextProperty, Function<MODEL, ObservableValue<W>> function) {
	// return Binding.<MODEL, V, ObservableValue<W>> bind(function, Binder.propertySetter(getTextProperty));
	// }
	//
	// public static <R, MODEL, V, W> Binding<MODEL, V, W> setProperty(Function<V, Property<R>> getProperty, R value) {
	// Function<MODEL, ObservableValue<R>> function = (u) -> new SimpleObjectProperty<R>(value);
	// return (Binding<MODEL, V, W>) Binding.bind(function, Binder.propertySetter(getProperty));
	// }

	public static <R, MODEL, SUBMODEL> Binding<MODEL, SUBMODEL, Property<String>> bindInputText(Function<R, Property<String>> getTextProperty, Function<MODEL, Property<String>> function) {
		return Binding.<MODEL, SUBMODEL, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <R, MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, Consumer<MODEL> function) {
		return Binding.<MODEL, SUBMODEL, T> bind(function, Binder.actionBinder(propAction));
	}

	public static <R, MODEL, SUBMODEL, T> Binding<MODEL, SUBMODEL, T> bindAction(Function<R, ObjectProperty<EventHandler<ActionEvent>>> propAction, BiConsumer<MODEL, SUBMODEL> function, Class<SUBMODEL> clazz) {
		return Binding.<MODEL, SUBMODEL, T> bind(function, Binder.actionBinder(propAction));
	}
}
