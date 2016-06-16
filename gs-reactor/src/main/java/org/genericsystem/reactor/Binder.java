package org.genericsystem.reactor;

import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.reactor.Element.HtmlDomNode;

import javafx.beans.binding.Bindings;
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
public interface Binder<X, Y> {

	default void init(Function<? extends HtmlDomNode, Y> applyOnNode, Function<? extends Model, X> applyOnModel, ModelContext modelContext, HtmlDomNode node) {
		init(((Function<HtmlDomNode, Y>) applyOnNode).apply(node), () -> applyOnModel.apply(modelContext.getModel()));
	}

	default void init(Y nodeResult, Supplier<X> applyOnModel) {
		init(nodeResult, applyOnModel.get());
	}

	default void init(Y nodeResult, X modelResult) {
	}

	@Deprecated
	public static <W, Y> Binder<ObservableValue<W>, Property<W>> propertyBinder() {
		return new Binder<ObservableValue<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, ObservableValue<W> modelResult) {
				nodeResult.bind(modelResult);
			}
		};
	}

	public static Binder<Consumer<Object>, Property<Consumer<Object>>> actionBinder() {
		return new Binder<Consumer<Object>, Property<Consumer<Object>>>() {
			@Override
			public void init(Property<Consumer<Object>> nodeResult, Supplier<Consumer<Object>> applyOnModel) {
				nodeResult.setValue(o -> applyOnModel.get());
			}
		};
	}

	@Deprecated
	public static <W> Binder<Property<W>, ObservableValue<W>> propertyReverseBinder() {
		return new Binder<Property<W>, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> nodeResult, Property<W> modelResult) {
				modelResult.bind(nodeResult);
			}
		};
	}

	@Deprecated
	public static <N, W> Binder<ObservableList<W>, Property<ObservableList<W>>> observableListPropertyBinder() {
		return new Binder<ObservableList<W>, Property<ObservableList<W>>>() {
			@Override
			public void init(Property<ObservableList<W>> nodeResult, ObservableList<W> modelResult) {
				nodeResult.setValue(modelResult);
			}
		};
	}

	public static <N, W> Binder<Property<W>, Property<W>> propertyBiDirectionalBinder() {
		return new Binder<Property<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, Property<W> modelResult) {
				nodeResult.bindBidirectional(modelResult);
			}
		};
	}

	@Deprecated
	public static Binder<ObservableSet<String>, Set<String>> observableSetBinder() {
		return new Binder<ObservableSet<String>, Set<String>>() {
			@Override
			public void init(Set<String> nodeResult, ObservableSet<String> modelResult) {
				Bindings.bindContent(nodeResult, modelResult);
			}
		};
	}

	@Deprecated
	public static Binder<ObservableMap<String, String>, Map<String, String>> observableMapBinder() {
		return new Binder<ObservableMap<String, String>, Map<String, String>>() {
			@Override
			public void init(Map<String, String> nodeResult, ObservableMap<String, String> modelResult) {
				Bindings.bindContent(nodeResult, modelResult);
			}
		};
	}

}
