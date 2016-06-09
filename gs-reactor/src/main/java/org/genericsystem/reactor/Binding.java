package org.genericsystem.reactor;

import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Element.HtmlDomNode;

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
public class Binding<X, Y> {

	private final Function<? extends HtmlDomNode, Y> applyOnNode;
	private final Function<Model, X> applyOnModel;

	private final Binder<X, Y> binder;

	public Binding(Function<? extends HtmlDomNode, Y> applyOnNode, Function<Model, X> applyOnModel, Binder<X, Y> binder) {
		this.applyOnNode = applyOnNode;
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ModelContext modelContext, HtmlDomNode node) {
		binder.init(applyOnNode, applyOnModel, modelContext, node);
	}

	@SuppressWarnings("unchecked")
	static <M, X, Y> Binding<X, Y> bind(Function<? extends HtmlDomNode, Y> applyOnNode, Function<M, X> applyOnModel, Binder<X, Y> binder) {
		return new Binding<>(applyOnNode, (u) -> applyOnModel.apply((M) u), binder);
	}

	@SuppressWarnings("unchecked")
	private static <M, X, Y> Binding<X, Y> bind(Function<? extends HtmlDomNode, Y> applyOnNode, Consumer<M> applyOnModel, Binder<X, Y> binder) {
		return new Binding<>(applyOnNode, (u) -> {
			applyOnModel.accept((M) u);
			return null;
		}, binder);
	}

	public static <M, W> Binding<ObservableValue<W>, Property<W>> bindProperty(Function<M, ObservableValue<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBinder());
	}

	public static <M, W> Binding<Property<W>, ObservableValue<W>> bindReversedProperty(Function<M, Property<W>> applyOnModel,
			Function<? extends HtmlDomNode, ObservableValue<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyReverseBinder());
	}

	public static <M, W> Binding<Property<W>, Property<W>> bindBiDirectionalProperty(Function<M, Property<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBiDirectionalBinder());
	}

	public static <M, W> Binding<W, Property<W>> bindAction(Consumer<M> applyOnModel, Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.actionBinder());
	}

	public static <M, W> Binding<ObservableValue<Boolean>, Set<W>> bindSet(Function<M, ObservableValue<Boolean>> applyOnModel, W styleClass,
			Function<? extends HtmlDomNode, Set<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableSetBinder(styleClass));
	}

	public static <M, W> Binding<ObservableList<W>, Property<ObservableList<W>>> bindObservableList(Function<M, ObservableList<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<ObservableList<W>>> applyOnNode) {
		return Binding.<M, ObservableList<W>, Property<ObservableList<W>>> bind(applyOnNode, applyOnModel, Binder.observableListPropertyBinder());
	}
}
