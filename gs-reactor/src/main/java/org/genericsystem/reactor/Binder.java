package org.genericsystem.reactor;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.collections.ObservableMap;
import javafx.collections.ObservableSet;
import javafx.event.EventHandler;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <X>
 * @param <Y>
 */
public interface Binder<N, X, Y> {

	default void init(Function<N, Y> applyOnNode, Function<Model, X> method, ModelContext modelContext, N node) {
		init(applyOnNode.apply(node), modelContext.applyOnModel(method), modelContext);
	}

	default void init(Y nodeResult, Supplier<X> applyOnModel, ModelContext modelContext) {
		init(nodeResult, applyOnModel.get());
	}

	default void init(Y nodeResult, X modelResult) {
	}

	public static <N, W, Y> Binder<N, ObservableValue<W>, Property<W>> propertyBinder() {
		return new Binder<N, ObservableValue<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, ObservableValue<W> modelResult) {
				nodeResult.bind(modelResult);
			}
		};
	}

	public static <N, W> Binder<N, W, Property<W>> actionBinder() {
		return new Binder<N, W, Property<W>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Property<W> nodeResult, Supplier<W> applyOnModel, ModelContext modelContext) {
				nodeResult.setValue((W) (EventHandler) event -> applyOnModel.get());
			}
		};
	}

	public static <N, W> Binder<N, Property<W>, ObservableValue<W>> propertyReverseBinder() {
		return new Binder<N, Property<W>, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> nodeResult, Property<W> modelResult) {
				modelResult.bind(nodeResult);
			}
		};
	}

	public static <N, W> Binder<N, ObservableList<W>, Property<ObservableList<W>>> observableListPropertyBinder() {
		return new Binder<N, ObservableList<W>, Property<ObservableList<W>>>() {
			@Override
			public void init(Property<ObservableList<W>> nodeResult, ObservableList<W> modelResult) {
				nodeResult.setValue(modelResult);
			}
		};
	}

	public static <N, W> Binder<N, Property<W>, Property<W>> propertyBiDirectionalBinder() {
		return new Binder<N, Property<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, Property<W> modelResult) {
				nodeResult.bindBidirectional(modelResult);
			}
		};
	}

	public static <N> Binder<N, ObservableValue<String>, ObservableSet<String>> observableSetBinder() {
		return new Binder<N, ObservableValue<String>, ObservableSet<String>>() {
			@Override
			public void init(ObservableSet<String> nodeResult, ObservableValue<String> modelResult) {
				nodeResult.add(modelResult.getValue());
				modelResult.addListener((o, ov, nv) -> {
					nodeResult.remove(ov);
					nodeResult.add(nv);
				});
			}
		};
	}

	public static <N, W> Binder<N, ObservableValue<Boolean>, ObservableSet<W>> observableSetBinder(W styleClass) {
		return new Binder<N, ObservableValue<Boolean>, ObservableSet<W>>() {
			@Override
			public void init(ObservableSet<W> nodeResult, ObservableValue<Boolean> modelResult) {
				Consumer<Boolean> consumer = bool -> {
					if (bool)
						nodeResult.add(styleClass);
					else
						nodeResult.remove(styleClass);
				};
				consumer.accept(modelResult.getValue());
				modelResult.addListener((o, ov, nv) -> consumer.accept(nv));
			}
		};
	}

	public static <N> Binder<N, ObservableValue<Number>, ObservableMap<String, String>> observableMapBinder(String attr, String[] value) {
		return new Binder<N, ObservableValue<Number>, ObservableMap<String, String>>() {
			@Override
			public void init(ObservableMap<String, String> nodeResult, ObservableValue<Number> modelResult) {
				Consumer<Number> consumer = number -> {

					if (number.intValue() < value.length)
						nodeResult.put(attr, value[number.intValue()]);
				};
				consumer.accept(modelResult.getValue());
				modelResult.addListener((o, ov, nv) -> consumer.accept(nv));
			}
		};
	}

	public static <N> Binder<N, ObservableMap<String, String>, ObservableMap<String, String>> observableMapBinder() {
		return new Binder<N, ObservableMap<String, String>, ObservableMap<String, String>>() {
			@Override
			public void init(ObservableMap<String, String> nodeResult, ObservableMap<String, String> modelResult) {
				Bindings.bindContent(nodeResult, modelResult);
			}
		};
	}

}
