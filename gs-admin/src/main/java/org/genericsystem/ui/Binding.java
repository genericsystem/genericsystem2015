package org.genericsystem.ui;

import java.util.function.BiConsumer;
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

	public Binding(Binder<N, T> binder, Function<?, T> method) {
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
			String s = "/";
			while (modelContext_ != null) {
				s += modelContext_.getModel() + "/";
				try {
					return method.apply(modelContext_.getModel());
				} catch (ClassCastException ignore) {
				}
				modelContext_ = modelContext_.getParent();
			}
			throw new IllegalStateException("Unable to resolve a method reference : " + method + " on stack : " + s);
		};
	}

	static <N, M, T> Binding<N, T> bind(Binder<N, T> binder, Function<M, T> function) {
		return new Binding<>(binder, (u) -> function.apply((M) u));
	}

	private static <N, M, T> Binding<N, T> bind(Consumer<M> function, Binder<N, T> binder) {
		return new Binding<>(binder, (u) -> {
			function.accept((M) u);
			return null;
		});
	}

	private static <SUPERMODEL, N, M, T> Binding<N, Function<SUPERMODEL, T>> bind(BiConsumer<SUPERMODEL, M> function, Binder<N, Function<SUPERMODEL, T>> binder) {
		return new Binding<>(binder, (m) -> (sm -> {
			function.accept(sm, (M) m);
			return null;
		}));
	}

	private static <SUPERMODEL, N, T> Binding<N, Function<T, SUPERMODEL>> pushBinding(BiConsumer<SUPERMODEL, T> function, Binder<N, Function<T, SUPERMODEL>> binder) {
		return new Binding<>(binder, (sm) -> (m -> {
			function.accept((SUPERMODEL) sm, m);
			return null;
		}));
	}

	private static <N, M, T> Binding<N, T> bind(Function<M, T> function, Binder<N, T> binder) {
		return new Binding<>(binder, (u) -> function.apply((M) u));
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

	public static <SUPERMODEL, N, M, T extends Event> Binding<N, Function<SUPERMODEL, T>> bindMetaAction(Function<N, Property<EventHandler<T>>> propAction, BiConsumer<SUPERMODEL, M> biconsumer) {
		return Binding.<SUPERMODEL, N, M, T> bind(biconsumer, Binder.metaActionBinder(propAction));
	}

	public static <N, M, W> Binding<N, ObservableValue<W>> bindProperty(Function<N, Property<W>> getProperty, Function<M, ObservableValue<W>> function) {
		return Binding.bind(Binder.propertyBinder(getProperty), function);
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, ObservableValue<W>>> bindSuperProperty(Function<N, Property<W>> getProperty, Function<SUPERMODEL, ObservableValue<W>> function) {
		return Binding.bind(Binder.<N, SUPERMODEL, W> superPropertyBinder(getProperty), m -> function);
	}

	public static <N, M, W> Binding<N, Property<W>> bindBiDirectionalProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> function) {
		return Binding.bind(Binder.propertyBiDirectionalBinder(getProperty), function);
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>> bindObservableList(Function<N, ObservableList<W>> getObservable, Function<M, ObservableValue<Boolean>> function, W styleClass) {
		return Binding.bind(Binder.observableListBinder(getObservable, styleClass), function);
	}

	public static <N, M> Binding<N, ObservableValue<String>> bindObservableListToObservableValue(Function<N, ObservableList<String>> getObservable, Function<M, ObservableValue<String>> function) {
		return Binding.bind(Binder.observableListBinder(getObservable), function);
	}

	public static <N, M, T> Binding<N, T> bindGenericAction(Function<N, ObjectProperty<T>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, T> bind(consumer, Binder.genericActionBinder(propAction));
	}

	public static <SUPERMODEL,N, M, T> Binding<N, Function<SUPERMODEL, T>> bindGenericMouseAction(Function<N, ObjectProperty<T>> propAction, BiConsumer<SUPERMODEL, M> biConsumer) {
		return Binding.<SUPERMODEL,N, M, T> bind(biConsumer, Binder.genericMouseActionBinder(propAction));
	}
	
	public static <N, M, T extends Event> Binding<N, T> bindAction(Function<N, ObjectProperty<EventHandler<T>>> propAction, Consumer<M> consumer) {
		return Binding.<N, M, T> bind(consumer, Binder.actionBinder(propAction));
	}

	public static <SUPERMODEL, N, T> Binding<N, Function<T, SUPERMODEL>> pushModelActionOnSuperModel(Function<N, ObjectProperty<Consumer<T>>> propAction, BiConsumer<SUPERMODEL, T> biconsumer) {
		return Binding.<SUPERMODEL, N, T> pushBinding(biconsumer, Binder.pushModelActionOnSuperModel(propAction));
	}

	public static <N, M, W> Binding<N, ObservableList<W>> bindObservableList(Function<N, Property<ObservableList<W>>> getProperty, Function<M, ObservableList<W>> function) {
		return Binding.bind(Binder.observableListPropertyBinder(getProperty), function);
	}
}
