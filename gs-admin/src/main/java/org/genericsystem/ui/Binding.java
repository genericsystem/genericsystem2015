package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public class Binding<N, T> {

	private final Function<?, T> method;
	private final Binder<N, T> binder;

	public Binding(Function<?, T> method, Binder<N, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
		Supplier<T> applyOnModel = applyOnModel(modelContext);
		binder.init(applyOnModel, modelContext, viewContext, childElement);
	}

	protected Supplier<T> applyOnModel(ModelContext modelContext) {
		return () -> {
			ModelContext modelContext_ = modelContext;
			while (modelContext_ != null) {
				try {
					return method.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
		};
	}

	private static <N, M, T> Binding<N, T> bind(Binder<N, T> binder, Function<M, T> function) {
		return new Binding<>((u) -> function.apply((M) u), binder);
	}

	private static <N, M, T> Binding<N, T> bind(Consumer<M> function, Binder<N, T> binder) {
		return new Binding<>((u) -> {
			function.accept((M) u);
			return null;
		}, binder);
	}

	private static <N, M, T> Binding<N, T> bind(Function<M, T> function, Binder<N, T> binder) {
		return new Binding<>((u) -> function.apply((M) u), binder);
	}

	public static <N, M, T> Binding<N, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return Binding.bind(Binder.foreachBinder(), function);
	}

	public static <N, M, T> Binding<N, ObservableValue<T>> selector(Function<M, ObservableValue<T>> function) {
		return Binding.bind(Binder.selectorBinder(), function);
	}

	public static <N, M, W> Binding<N, Property<W>> bindReversedProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(function, Binder.propertyReverseBinder(getProperty));
	}

	public static <N, M, W> Binding<N, ObservableValue<W>> bindProperty(Function<N, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.bind(Binder.propertyBinder(getProperty), function);
	}

	public static <N, M, W> Binding<N, Property<W>> bindBiDirectionalProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(Binder.propertyBiDirectionalBinder(getProperty), function);
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>> bindObservableList(Function<N, ObservableList<W>> getObservable, Function<M, ObservableValue<Boolean>> function, W styleClass) {
		return Binding.bind(Binder.observableListBinder(getObservable, styleClass), function);
	}

	public static <N, M, T> Binding<N, T> bindGenericAction(Function<N, ObjectProperty<T>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, T> bind(consumer, Binder.genericActionBinder(propAction));
	}

	public static <N, M, T extends Event> Binding<N, T> bindAction(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, T> bind(consumer, Binder.actionBinder(propAction));
	}
}
