package org.genericsystem.distributed.ui;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <X>
 * @param <Y>
 */
public class Binding<N, X, Y> {

	private final Function<N, Y> applyOnNode;
	private final Function<Model, X> applyOnModel;
	private final Binder<N, X, Y> binder;

	public Binding(Function<N, Y> applyOnNode, Function<Model, X> applyOnModel, Binder<N, X, Y> binder) {
		this.applyOnNode = applyOnNode;
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ModelContext modelContext, N node) {
		binder.init(applyOnNode, applyOnModel, modelContext, node);
	}

	@SuppressWarnings("unchecked")
	static <N, M, X, Y> Binding<N, X, Y> bind(Function<N, Y> applyOnNode, Function<M, X> applyOnModel, Binder<N, X, Y> binder) {
		return new Binding<>(applyOnNode, (u) -> applyOnModel.apply((M) u), binder);
	}

	@SuppressWarnings("unchecked")
	private static <N, M, X, Y> Binding<N, X, Y> bind(Function<N, Y> applyOnNode, Consumer<M> applyOnModel, Binder<N, X, Y> binder) {
		return new Binding<>(applyOnNode, (u) -> {
			applyOnModel.accept((M) u);
			return null;
		}, binder);
	}

	public static <N, M, W> Binding<N, ObservableValue<W>, Property<W>> bindProperty(Function<M, ObservableValue<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBinder());
	}

	public static <N, M, W> Binding<N, Property<W>, ObservableValue<W>> bindReversedProperty(Function<M, Property<W>> applyOnModel, Function<N, ObservableValue<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyReverseBinder());
	}

	public static <N, M, W> Binding<N, Property<W>, Property<W>> bindBiDirectionalProperty(Function<M, Property<W>> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBiDirectionalBinder());
	}

	public static <N, M, W> Binding<N, W, Property<W>> bindAction(Consumer<M> applyOnModel, Function<N, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.actionBinder());
	}

	public static <N, M, W> Binding<N, ObservableValue<Boolean>, ObservableList<W>> bindObservableList(Function<M, ObservableValue<Boolean>> applyOnModel, W styleClass, Function<N, ObservableList<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableListBinder(applyOnNode, styleClass));
	}

	public static <N, M> Binding<N, ObservableValue<String>, ObservableList<String>> bindObservableListToObservableValue(Function<M, ObservableValue<String>> applyOnModel, Function<N, ObservableList<String>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableListBinder());
	}

	public static <N, M, W> Binding<N, ObservableList<W>, Property<ObservableList<W>>> bindObservableList(Function<M, ObservableList<W>> applyOnModel, Function<N, Property<ObservableList<W>>> applyOnNode) {
		return Binding.<N, M, ObservableList<W>, Property<ObservableList<W>>> bind(applyOnNode, applyOnModel, Binder.observableListPropertyBinder());
	}
}
