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

	private final Function<?, W> applyOnModel;
	private final Binder<N, W> binder;

	public Binding(Function<?, W> applyOnModel, Binder<N, W> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ModelContext modelContext, N node) {
		binder.init(applyOnModel, modelContext, node);
	}

	@SuppressWarnings("unchecked")
	static <N, M, W> Binding<N, W> bind(Function<M, W> applyOnModel, Binder<N, W> binder) {
		return new Binding<>((u) -> applyOnModel.apply((M) u), binder);
	}

	@SuppressWarnings("unchecked")
	private static <N, M, W> Binding<N, W> bind(Consumer<M> applyOnModel, Binder<N, W> binder) {
		return new Binding<>((u) -> {
			applyOnModel.accept((M) u);
			return null;
		}, binder);
	}

	@SuppressWarnings("unchecked")
	private static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, W>> bind(BiConsumer<SUPERMODEL, M> applyOnModel, Binder<N, Function<SUPERMODEL, W>> binder) {
		return new Binding<>((m) -> (sm -> {
			applyOnModel.accept(sm, (M) m);
			return null;
		}), binder);
	}

	@SuppressWarnings("unchecked")
	private static <SUPERMODEL, N, W> Binding<N, Function<W, SUPERMODEL>> pushBinding(BiConsumer<SUPERMODEL, W> applyOnModel, Binder<N, Function<W, SUPERMODEL>> binder) {
		return new Binding<>((sm) -> (m -> {
			applyOnModel.accept((SUPERMODEL) sm, m);
			return null;
		}), binder);
	}

	public static <N, M, W> Binding<N, ObservableValue<W>> bindProperty(Function<M, ObservableValue<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.propertyBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, Property<W>> bindReversedProperty(Function<M, Property<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.propertyReverseBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, Property<W>> bindBiDirectionalProperty(Function<M, Property<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.propertyBiDirectionalBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, ObservableValue<W>>> bindMetaProperty(Function<SUPERMODEL, ObservableValue<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(m -> applyOnModel, Binder.<N, SUPERMODEL, W> metaPropertyBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, W> bindAction(Consumer<M> consumer, Function<N, Property<W>> propAction) {
		return Binding.bind(consumer, Binder.actionBinder(propAction));
	}

	public static <SUPERMODEL, N, M, W extends Event> Binding<N, Function<SUPERMODEL, W>> bindMetaAction(BiConsumer<SUPERMODEL, M> applyOnModel, Function<N, Property<EventHandler<W>>> applyOnNode) {
		return Binding.<SUPERMODEL, N, M, W> bind(applyOnModel, Binder.metaActionBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>> bindObservableList(Function<M, ObservableValue<Boolean>> applyOnModel, W styleClass, Function<N, ObservableList<W>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.observableListBinder(applyOnNode, styleClass));
	}

	public static <N, M> Binding<N, ObservableValue<String>> bindObservableListToObservableValue(Function<M, ObservableValue<String>> applyOnModel, Function<N, ObservableList<String>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.observableListBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, W>> bindGenericMouseAction(BiConsumer<SUPERMODEL, M> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.<SUPERMODEL, N, M, W> bind(applyOnModel, Binder.genericMouseActionBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, W> Binding<N, Function<W, SUPERMODEL>> pushModelActionOnSuperModel(BiConsumer<SUPERMODEL, W> applyOnModel, Function<N, Property<Consumer<W>>> applyOnNode) {
		return Binding.<SUPERMODEL, N, W> pushBinding(applyOnModel, Binder.pushModelActionOnSuperModel(applyOnNode));
	}

	public static <N, M, W> Binding<N, ObservableList<W>> bindObservableList(Function<M, ObservableList<W>> applyOnModel, Function<N, Property<ObservableList<W>>> applyOnNode) {
		return Binding.bind(applyOnModel, Binder.observableListPropertyBinder(applyOnNode));
	}
}
