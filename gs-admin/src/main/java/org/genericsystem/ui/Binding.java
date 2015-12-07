package org.genericsystem.ui;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public class Binding<M, SUBMODEL, T> {

	private final BiFunction<M, SUBMODEL, T> method;
	private final Binder<SUBMODEL, T> binder;

	public Binding(BiFunction<M, SUBMODEL, T> method, Binder<SUBMODEL, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	@SuppressWarnings("unchecked")
	public void init(ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
		Function<? super SUBMODEL, T> applyOnModel = applyOnModel(modelContext);
		T wrapper = applyOnModel.apply((SUBMODEL) modelContext.getModel());
		binder.init(wrapper, modelContext, viewContext, childElement);
	}

	protected Function<? super SUBMODEL, T> applyOnModel(ModelContext<M> modelContext) {
		return (SUBMODEL) -> {
			ModelContext<?> modelContext_ = modelContext;
			while (modelContext_ != null) {
				try {
					return method.apply((M) modelContext_.getModel(), SUBMODEL);
				} catch (ClassCastException ignore) {}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
		};
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(Function<M, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> function.apply(u), binder);
	}

	private static <M, SUBMODEL, T> Binding<M, SUBMODEL, T> bind(BiFunction<M, SUBMODEL, T> function, Binder<SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> function.apply(u, v), binder);
	}

	public static <M, SUBMODEL, T> Binding<M, SUBMODEL, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return Binding.bind(function, Binder.foreachBinder());
	}

	public static <R, M, V, W> Binding<M, V, ObservableValue<W>> bindProperty(Function<R, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.bind(function, Binder.propertyBinder(getProperty));
	}

	public static <R, M, SUBMODEL> Binding<M, SUBMODEL, Property<String>> bindInputText(Function<R, Property<String>> getTextProperty, Function<M, Property<String>> function) {
		return Binding.<M, SUBMODEL, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <R, M, SUBMODEL, T extends Event> Binding<M, SUBMODEL, ObjectProperty<EventHandler<T>>> bindAction(Function<R, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		return Binding.<M, SUBMODEL, ObjectProperty<EventHandler<T>>> bind(m -> new SimpleObjectProperty<>(t -> consumer.accept(m)), Binder.actionBinder(propAction));
	}

	public static <R, M, SUBMODEL, T extends Event> Binding<M, SUBMODEL, ObjectProperty<EventHandler<T>>> bindAction(Function<R, ObjectProperty<EventHandler<T>>> propAction, BiConsumer<M, SUBMODEL> biConsumer, Class<SUBMODEL> clazz) {
		return Binding.<M, SUBMODEL, ObjectProperty<EventHandler<T>>> bind((m, sm) -> {
			return new SimpleObjectProperty<>(t -> biConsumer.accept(m, sm));
		}, Binder.actionBinder(propAction));
	}
}
