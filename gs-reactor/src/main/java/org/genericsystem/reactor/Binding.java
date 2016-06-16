package org.genericsystem.reactor;

import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;

import org.genericsystem.reactor.Element.HtmlDomNode;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;

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
		private final Function<? extends Model, X> applyOnModel;

		private final Binder<X, Y> binder;

		public BindingImpl(Function<? extends HtmlDomNode, Y> applyOnNode, Function<? extends Model, X> applyOnModel, Binder<X, Y> binder) {
			this.applyOnNode = applyOnNode;
			this.applyOnModel = applyOnModel;
			this.binder = binder;
		}

		@Override
		public void init(ModelContext modelContext, HtmlDomNode node) {
			binder.init(applyOnNode, applyOnModel, modelContext, node);
		}
	}

	static <X, Y> Binding bind(Function<? extends HtmlDomNode, Y> applyOnNode, Function<? extends Model, X> applyOnModel, Binder<X, Y> binder) {
		return new BindingImpl<>(applyOnNode, applyOnModel, binder);
	}

	@SuppressWarnings("unchecked")
	public static <M extends Model, X, Y> Binding bind(Function<? extends HtmlDomNode, Y> applyOnNode, Consumer<M> applyOnModel, Binder<X, Y> binder) {
		return new BindingImpl<>(applyOnNode, model -> {
			applyOnModel.accept((M) model);
			return null;
		}, binder);
	}

	@Deprecated
	public static <W> Binding bindProperty(Function<? extends Model, ObservableValue<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBinder());
	}

	@Deprecated
	public static <W> Binding bindReversedProperty(Function<? extends Model, Property<W>> applyOnModel,
			Function<? extends HtmlDomNode, ObservableValue<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyReverseBinder());
	}

	public static <W> Binding bindBiDirectionalProperty(Function<? extends Model, Property<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<W>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.propertyBiDirectionalBinder());
	}

	public static Binding bindAction(Consumer<? extends Model> applyOnModel, Function<? extends HtmlDomNode, Property<Consumer<Object>>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.actionBinder());
	}

	// @Deprecated
	// public static Binding bindStyleClass(Element<?> element, Function<? extends Model, ObservableValue<Boolean>> applyOnModel, String styleClass) {
	// return Binding.bind(null, applyOnModel, Binder.styleClassBinder(element, styleClass));
	// }

	@Deprecated
	public static <W> Binding bindSet(Function<? extends Model, ObservableSet<String>> applyOnModel, Function<? extends HtmlDomNode, Set<String>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableSetBinder());
	}

	@Deprecated
	public static <W> Binding bindObservableList(Function<? extends Model, ObservableList<W>> applyOnModel,
			Function<? extends HtmlDomNode, Property<ObservableList<W>>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableListPropertyBinder());
	}

	@Deprecated
	public static Binding bindMap(Function<? extends Model, ObservableMap<String, String>> applyOnModel,
			Function<? extends HtmlDomNode, Map<String, String>> applyOnNode) {
		return Binding.bind(applyOnNode, applyOnModel, Binder.observableMapBinder());
	}

	public static Binding bindInit(Consumer<? extends Model> consumer) {
		return (modelContext, node) -> consumer.accept(modelContext.getModel());
	}
}
