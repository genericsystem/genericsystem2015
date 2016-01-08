package org.genericsystem.ui;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public class Binding<N, W> {

	private final Function<?, W> method;
	private final Binder<N, W> binder;

	public Binding(Binder<N, W> binder, Function<?, W> method) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, N node) {
		binder.init(method, modelContext, node);
	}

	static <N, M, W> Binding<N, W> bind(Binder<N, W> binder, Function<M, W> function) {
		return new Binding<>(binder, (u) -> function.apply((M) u));
	}

	private static <N, M, W> Binding<N, W> bind(Consumer<M> function, Binder<N, W> binder) {
		return new Binding<>(binder, (u) -> {
			function.accept((M) u);
			return null;
		});
	}

	private static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, W>> bind(BiConsumer<SUPERMODEL, M> function, Binder<N, Function<SUPERMODEL, W>> binder) {
		return new Binding<>(binder, (m) -> (sm -> {
			function.accept(sm, (M) m);
			return null;
		}));
	}

	private static <SUPERMODEL, N, W> Binding<N, Function<W, SUPERMODEL>> pushBinding(BiConsumer<SUPERMODEL, W> function, Binder<N, Function<W, SUPERMODEL>> binder) {
		return new Binding<>(binder, (sm) -> (m -> {
			function.accept((SUPERMODEL) sm, m);
			return null;
		}));
	}

	private static <N, M, W> Binding<N, W> bind(Function<M, W> function, Binder<N, W> binder) {
		return new Binding<>(binder, (u) -> function.apply((M) u));
	}

	public static <N, M, W> Binding<N, ObservableValue<W>> bindProperty(Function<N, Property<W>> getProperty, Function<M, ObservableValue<W>> applyOnNode) {
		return Binding.bind(Binder.propertyBinder(getProperty), applyOnNode);
	}

	public static <N, M, W> Binding<N, Property<W>> bindReversedProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, Binder.propertyReverseBinder(getProperty));
	}

	public static <N, M, W> Binding<N, Property<W>> bindBiDirectionalProperty(Function<N, Property<W>> getProperty, Function<M, Property<W>> applyOnNode) {
		return Binding.bind(Binder.propertyBiDirectionalBinder(getProperty), applyOnNode);
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, ObservableValue<W>>> bindMetaProperty(Function<N, Property<W>> getProperty, Function<SUPERMODEL, ObservableValue<W>> applyOnNode) {
		return Binding.bind(Binder.<N, SUPERMODEL, W> metaPropertyBinder(getProperty), m -> applyOnNode);
	}

	public static <N, M, W> Binding<N, W> bindAction(Function<N, Property<W>> propAction, Consumer<M> consumer) {
		return Binding.bind(consumer, Binder.actionBinder(propAction));
	}

	public static <SUPERMODEL, N, M, W extends Event> Binding<N, Function<SUPERMODEL, W>> bindMetaAction(Function<N, Property<EventHandler<W>>> propAction, BiConsumer<SUPERMODEL, M> biconsumer) {
		return Binding.<SUPERMODEL, N, M, W> bind(biconsumer, Binder.metaActionBinder(propAction));
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>> bindObservableList(Function<N, ObservableList<W>> getObservable, Function<M, ObservableValue<Boolean>> applyOnNode, W styleClass) {
		return Binding.bind(Binder.observableListBinder(getObservable, styleClass), applyOnNode);
	}

	public static <N, M> Binding<N, ObservableValue<String>> bindObservableListToObservableValue(Function<N, ObservableList<String>> getObservable, Function<M, ObservableValue<String>> applyOnNode) {
		return Binding.bind(Binder.observableListBinder(getObservable), applyOnNode);
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, W>> bindGenericMouseAction(Function<N, Property<W>> propAction, BiConsumer<SUPERMODEL, M> biConsumer) {
		return Binding.<SUPERMODEL, N, M, W> bind(biConsumer, Binder.genericMouseActionBinder(propAction));
	}

	public static <SUPERMODEL, N, W> Binding<N, Function<W, SUPERMODEL>> pushModelActionOnSuperModel(Function<N, Property<Consumer<W>>> propAction, BiConsumer<SUPERMODEL, W> biconsumer) {
		return Binding.<SUPERMODEL, N, W> pushBinding(biconsumer, Binder.pushModelActionOnSuperModel(propAction));
	}

	public static <N, M, W> Binding<N, ObservableList<W>> bindObservableList(Function<N, Property<ObservableList<W>>> getProperty, Function<M, ObservableList<W>> applyOnNode) {
		return Binding.bind(Binder.observableListPropertyBinder(getProperty), applyOnNode);
	}
}
