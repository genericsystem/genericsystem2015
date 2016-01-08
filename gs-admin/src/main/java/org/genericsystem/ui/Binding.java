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

	public Binding(Function<?, W> method, Binder<N, W> binder) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, N node) {
		binder.init(method, modelContext, node);
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

	public static <N, M, W> Binding<N, ObservableValue<W>> bindProperty(Function<N, Property<W>> applyOnNode, Function<M, ObservableValue<W>> applyOnModel) {
		return Binding.bind(applyOnModel, Binder.propertyBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, Property<W>> bindReversedProperty(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		return Binding.bind(applyOnModel, Binder.propertyReverseBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, Property<W>> bindBiDirectionalProperty(Function<N, Property<W>> applyOnNode, Function<M, Property<W>> applyOnModel) {
		return Binding.bind(applyOnModel, Binder.propertyBiDirectionalBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, ObservableValue<W>>> bindMetaProperty(Function<N, Property<W>> applyOnNode, Function<SUPERMODEL, ObservableValue<W>> applyOnModel) {
		return Binding.bind(m -> applyOnModel, Binder.<N, SUPERMODEL, W> metaPropertyBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, W> bindAction(Function<N, Property<W>> propAction, Consumer<M> consumer) {
		return Binding.bind(consumer, Binder.actionBinder(propAction));
	}

	public static <SUPERMODEL, N, M, W extends Event> Binding<N, Function<SUPERMODEL, W>> bindMetaAction(Function<N, Property<EventHandler<W>>> applyOnNode, BiConsumer<SUPERMODEL, M> applyOnModel) {
		return Binding.<SUPERMODEL, N, M, W> bind(applyOnModel, Binder.metaActionBinder(applyOnNode));
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>> bindObservableList(Function<N, ObservableList<W>> applyOnNode, Function<M, ObservableValue<Boolean>> applyOnModel, W styleClass) {
		return Binding.bind(applyOnModel, Binder.observableListBinder(applyOnNode, styleClass));
	}

	public static <N, M> Binding<N, ObservableValue<String>> bindObservableListToObservableValue(Function<N, ObservableList<String>> applyOnNode, Function<M, ObservableValue<String>> applyOnModel) {
		return Binding.bind(applyOnModel, Binder.observableListBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, M, W> Binding<N, Function<SUPERMODEL, W>> bindGenericMouseAction(Function<N, Property<W>> applyOnNode, BiConsumer<SUPERMODEL, M> applyOnModel) {
		return Binding.<SUPERMODEL, N, M, W> bind(applyOnModel, Binder.genericMouseActionBinder(applyOnNode));
	}

	public static <SUPERMODEL, N, W> Binding<N, Function<W, SUPERMODEL>> pushModelActionOnSuperModel(Function<N, Property<Consumer<W>>> applyOnNode, BiConsumer<SUPERMODEL, W> applyOnModel) {
		return Binding.<SUPERMODEL, N, W> pushBinding(applyOnModel, Binder.pushModelActionOnSuperModel(applyOnNode));
	}

	public static <N, M, W> Binding<N, ObservableList<W>> bindObservableList(Function<N, Property<ObservableList<W>>> applyOnNode, Function<M, ObservableList<W>> applyOnModel) {
		return Binding.bind(applyOnModel, Binder.observableListPropertyBinder(applyOnNode));
	}
}
