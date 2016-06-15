package org.genericsystem.reactor;

import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

import org.genericsystem.reactor.Element.HtmlDomNode;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <X>
 * @param <Y>
 */
public interface Binding {
	public void init(ModelContext modelContext, HtmlDomNode node);

	public static class BindingImpl<X, Y> implements Binding {

		private final Function<? extends HtmlDomNode, Y> applyOnNode;
		private final Function<Model, X> applyOnModel;

		private final Binder<X, Y> binder;

		public BindingImpl(Function<? extends HtmlDomNode, Y> applyOnNode, Function<Model, X> applyOnModel, Binder<X, Y> binder) {
			this.applyOnNode = applyOnNode;
			this.applyOnModel = applyOnModel;
			this.binder = binder;
		}

		@Override
		public void init(ModelContext modelContext, HtmlDomNode node) {
			binder.init(applyOnNode, applyOnModel, modelContext, node);
		}
	}

	@SuppressWarnings("unchecked")
	static <M, X, Y> Binding bind(Function<? extends HtmlDomNode, Y> applyOnNode, Function<M, X> applyOnModel, Binder<X, Y> binder) {
		return new BindingImpl<>(applyOnNode, (u) -> applyOnModel.apply((M) u), binder);
	}

	@SuppressWarnings("unchecked")
	public static <M, X, Y> Binding bind(Function<? extends HtmlDomNode, Y> applyOnNode, Consumer<M> applyOnModel, Binder<X, Y> binder) {
		return new BindingImpl<>(applyOnNode, (u) -> {
			applyOnModel.accept((M) u);
			return null;
		}, binder);
	}

	public static <M, W> Binding bindProperty(Function<M, ObservableValue<W>> applyOnModel, Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBinder());
	}

	public static <M, W> Binding bindReversedProperty(Function<M, Property<W>> applyOnModel, Function<? extends HtmlDomNode, ObservableValue<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyReverseBinder());
	}

	public static <M, W> Binding bindBiDirectionalProperty(Function<M, Property<W>> applyOnModel, Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBiDirectionalBinder());
	}

	public static <M, W> Binding bindAction(Consumer<M> applyOnModel, Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.actionBinder());
	}

	@Deprecated
	public static <M> Binding bindStyleClass(Element<?> element, Function<M, ObservableValue<Boolean>> applyOnModel, String styleClass) {
		return Binding.bind(null, applyOnModel, Binder.styleClassBinder(element, styleClass));
	}

	public static <M, W> Binding bindSet(Function<M, ObservableSet<String>> applyOnModel, Function<? extends HtmlDomNode, Set<String>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableSetBinder());
	}

	public static <M, W> Binding bindObservableList(Function<M, ObservableList<W>> applyOnModel, Function<? extends HtmlDomNode, Property<ObservableList<W>>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableListPropertyBinder());
	}

	public static <M> Binding bindMap(Function<M, ObservableMap<String, String>> applyOnModel, Function<? extends HtmlDomNode, Map<String, String>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableMapBinder());
	}

	public static Binding bindInit(Consumer<Model> consumer) {
		return (modelContext, node) -> consumer.accept(modelContext.getModel());
	}
}
