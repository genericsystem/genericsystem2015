package org.genericsystem.ui;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public class Binding<N, SUBMODEL, T> {

	private final Function<?, T> method;
	private final Binder<N, SUBMODEL, T> binder;

	public Binding(Function<?, T> method, Binder<N, SUBMODEL, T> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<N> viewContext, Element<SUBMODEL> childElement) {
		Supplier<T> applyOnModel = applyOnModel(modelContext);
		binder.init(applyOnModel, modelContext, viewContext, childElement);
	}

	protected Supplier<T> applyOnModel(ModelContext modelContext) {
		return () -> {
			ModelContext modelContext_ = modelContext;
			while (modelContext_ != null) {
				try {
					return method.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {
				}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on : " + modelContext.getModel());
		};
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(Binder<N, SUBMODEL, T> binder, Function<M, T> function) {
		return new Binding<>((u) -> function.apply((M) u), binder);
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(Consumer<M> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u) -> {
			function.accept((M) u);
			return null;
		}, binder);
	}

	private static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bind(Function<M, T> function, Binder<N, SUBMODEL, T> binder) {
		return new Binding<>((u) -> function.apply((M) u), binder);
	}

	public static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return Binding.bind(Binder.foreachBinder(), function);
	}

	public static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, ObservableValue<T>> selector(Function<M, ObservableValue<T>> function) {
		return Binding.bind(Binder.selectorBinder(), function);
	}

	public static <N, M, SUBMODEL, W> Binding<N, SUBMODEL, Property<W>> bindReversedProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(function, Binder.propertyReverseBinder(getProperty));
	}

	public static <N, M, SUBMODEL, W> Binding<N, SUBMODEL, ObservableValue<W>> bindProperty(Function<N, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.bind(Binder.propertyBinder(getProperty), function);
	}

	public static <N, M, SUBMODEL, W> Binding<N, SUBMODEL, Property<W>> bindBiDirectionalProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(Binder.propertyBiDirectionalBinder(getProperty), function);
	}

	public static <N, M, SUBMODEL, W> Binding<N, SUBMODEL, Property<Boolean>> bindObservableList(Function<N, ObservableList<W>> getObservable, Function<M, Property<Boolean>> function, W styleClass) {
		return Binding.bind(Binder.observableListBinder(getObservable, styleClass), function);
	}

	static <N, T> Function<N, ObjectProperty<Consumer<Event>>> toObjectPropertyConsumer(Function<N, ObjectProperty<T>> f) {
		return label -> new SimpleObjectProperty<Consumer<Event>>() {
			@Override
			public void set(Consumer<Event> consumer) {
				f.apply(label).set((T) (EventHandler) consumer::accept);
			}
		};
	}

	public static <N, M, SUBMODEL, T> Binding<N, SUBMODEL, T> bindGenericAction(Function<N, ObjectProperty<T>> propAction, Consumer<M> consumer) {
		Function<N, ObjectProperty<Consumer<Event>>> objectPropertyConsumer = toObjectPropertyConsumer(propAction);
		return Binding.<N, M, SUBMODEL, T> bind(consumer, Binder.genericActionBinder(objectPropertyConsumer));
	}

	public static <N, M, SUBMODEL, T extends Event> Binding<N, SUBMODEL, T> bindAction(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, SUBMODEL, T> bind(consumer, Binder.actionBinder(propAction));
	}
}
