package org.genericsystem.ui;

import java.util.function.BiConsumer;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public class Binding<N, SUBMODEL, T> {

	private final BiFunction<?, SUBMODEL, T> method;
	private final Binder<N, SUBMODEL, T> binder;

	public Binding(BiFunction<?, SUBMODEL, T> method, Binder<N, SUBMODEL, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<N> viewContext, Element<SUBMODEL> childElement) {
		Function<? super SUBMODEL, T> applyOnModel = applyOnModel(modelContext);
		binder.init(applyOnModel, modelContext, viewContext, childElement);
	}

	protected Function<? super SUBMODEL, T> applyOnModel(ModelContext modelContext) {
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

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(Function<M, T> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> function.apply((M) u), binder);
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(BiFunction<M, SUBMODEL, T> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> function.apply((M) u, v), binder);
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(Consumer<M> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> {
			function.accept((M) u);
			return null;
		}, binder);
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(BiConsumer<M, SUBMODEL> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u, v) -> {
			function.accept((M) u, v);
			return null;
		}, binder);
	}

	public static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return Binding.bind(function, Binder.foreachBinder());
	}

	public static <N, M, V, W> Binding<N, V, ObservableValue<W>> bindProperty(Function<N, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.bind(function, Binder.propertyBinder(getProperty));
	}

	public static <N, M, V, W> Binding<N, V, Property<W>> bindReversedProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(function, Binder.propertyReverseBinder(getProperty));
	}

	public static <N, M, SUBMODEL> Binding<N, SUBMODEL, Property<String>> bindInputText(Function<N, Property<String>> getTextProperty, Function<M, Property<String>> function) {
		return Binding.<N, M, SUBMODEL, Property<String>> bind(function, Binder.inputTextBinder(getTextProperty));
	}

	public static <N, M, SUBMODEL, T extends Event> Binding<N, SUBMODEL, T> bindAction(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, SUBMODEL, T> bind(consumer, Binder.actionBinder(propAction));
	}

	public static <N, M, SUBMODEL, T extends Event> Binding<N, SUBMODEL, T> bindAction(Function<N, ObjectProperty<EventHandler<T>>> propAction, BiConsumer<M, SUBMODEL> biConsumer, Class<SUBMODEL> clazz) {
		return Binding.<N, M, SUBMODEL, T> bind(biConsumer, Binder.actionBinder(propAction));
	}
}
