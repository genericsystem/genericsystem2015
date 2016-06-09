package org.genericsystem.reactor;

import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.reactor.Element.HtmlDomNode;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.EventHandler;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <X>
 * @param <Y>
 */
public interface Binder<X, Y> {

	default void init(Function<? extends HtmlDomNode, Y> applyOnNode, Function<Model, X> method, ModelContext modelContext, HtmlDomNode node) {
		init(((Function<HtmlDomNode, Y>) applyOnNode).apply(node), modelContext.applyOnModel(method), modelContext);
	}

	default void init(Y nodeResult, Supplier<X> applyOnModel, ModelContext modelContext) {
		init(nodeResult, applyOnModel.get());
	}

	default void init(Y nodeResult, X modelResult) {
	}

	public static <W, Y> Binder<ObservableValue<W>, Property<W>> propertyBinder() {
		return new Binder<ObservableValue<W>, Property<W>>() {
			@Override
			public void init(Property<W> nodeResult, ObservableValue<W> modelResult) {
				nodeResult.bind(modelResult);
			}
		};
	}

	public static <W> Binder<W, Property<W>> actionBinder() {
		return new Binder<W, Property<W>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Property<W> nodeResult, Supplier<W> applyOnModel, ModelContext modelContext) {
				nodeResult.setValue((W) (EventHandler) event -> applyOnModel.get());
			}
		};
	}

	public static <W> Binder<Property<W>, ObservableValue<W>> propertyReverseBinder() {
		return new Binder<Property<W>, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> nodeResult, Property<W> modelResult) {
				modelResult.bind(nodeResult);
			}
		};
	}

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

	public static <W> Binder<ObservableValue<Boolean>, Set<W>> observableSetBinder(W styleClass) {
		return new Binder<ObservableValue<Boolean>, Set<W>>() {
			@Override
			public void init(Set<W> nodeResult, ObservableValue<Boolean> modelResult) {
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

}
