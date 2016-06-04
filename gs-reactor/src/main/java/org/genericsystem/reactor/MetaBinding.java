package org.genericsystem.reactor;

import java.util.function.Function;
import java.util.function.Supplier;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.composite.CompositeModel;
import org.genericsystem.reactor.composite.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.composite.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.composite.CompositeModel.StringExtractor;

import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <T>
 */
public class MetaBinding<N, T extends ObservableList<?>> {

	private final Function<Model, T> applyOnModel;
	private final MetaBinder<N, T> binder;

	public MetaBinding(Function<Model, T> applyOnModel, MetaBinder<N, T> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ViewContext<?, N> viewContext, Element<?, ?> childElement) {
		binder.init(applyOnModel, viewContext, childElement);
	}

	@Override
	protected void finalize() throws Throwable {
		System.out.println("FINALIZE22222222222222222222222222");
	};

	public static <N, M extends Model, SUBMODEL extends Model> MetaBinding<N, ObservableList<SUBMODEL>> forEach(
			Function<M, ObservableList<SUBMODEL>> applyOnModel) {
		return new MetaBinding<>(model -> applyOnModel.apply((M) model), MetaBinder.<N, SUBMODEL> foreachBinder());
	}

	// public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> forEach(Function<M, ObservableList<CompositeModel>>
	// applyOnModel, StringExtractor stringExtractor, ObservableListExtractor observableListExtractor,
	// ModelConstructor<CompositeModel> constructor) {
	// return forEach(model -> ((M) model).getBoundObservableList(applyOnModel, stringExtractor, observableListExtractor, constructor));
	// }

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> forEach(Element<?, ?> element, StringExtractor stringExtractor,
			ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		return forEach(model -> ((CompositeModel) model).getBoundObservableList(element, stringExtractor, observableListExtractor, constructor));
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> selector(Element<?, ?> element, StringExtractor stringExtractor,
			Supplier<Generic> genericSupplier, ModelConstructor<CompositeModel> constructor) {
		return forEach(model -> ((CompositeModel) model).getBoundObservableList(element, stringExtractor, genericSupplier, constructor));
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> selector(Element<?, ?> element, StringExtractor stringExtractor,
			Class<?> genericClass, ModelConstructor<CompositeModel> constructor) {
		return forEach(model -> ((CompositeModel) model).getBoundObservableList(element, stringExtractor, genericClass, constructor));
	}

	public static <N, M extends Model, T extends Model> MetaBinding<N, ObservableList<T>> selector(Function<M, ObservableValue<T>> applyOnModel) {
		return forEach(model -> {
			ObservableValue<T> observableValue = applyOnModel.apply((M) model);
			return new ListBinding<T>() {
				{
					bind(observableValue);
				}

				@Override
				protected ObservableList<T> computeValue() {
					T value = observableValue.getValue();
					return value != null ? FXCollections.singletonObservableList(value) : FXCollections.emptyObservableList();
				}
			};
		});
	}

}
