package org.genericsystem.reactor;

import java.util.function.Function;
import java.util.function.Supplier;
import javafx.beans.binding.ListBinding;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;

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

	public static <N, M extends Model, SUBMODEL extends Model> MetaBinding<N, ObservableList<SUBMODEL>> forEach(Function<M, ObservableList<SUBMODEL>> applyOnModel) {
		return new MetaBinding<>(model -> applyOnModel.apply((M) model), MetaBinder.<N, SUBMODEL> foreachBinder());
	}

	public static <N> MetaBinding<N, ObservableList<CompositeModel>> forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		return new MetaBinding<>(model -> ((CompositeModel) model).getSubModels(), MetaBinder.<N> foreachBinder(stringExtractor, observableListExtractor, constructor));
	}

	public static <N, M extends Model, T extends Model> MetaBinding<N, ObservableList<T>> selector(Function<M, ObservableValue<T>> applyOnModel) {
		Function<M, ObservableList<T>> applyOnModelList = m -> new ListBinding<T>() {
			{
				bind(applyOnModel.apply(m));
			}

			@Override
			protected ObservableList<T> computeValue() {
				T value = applyOnModel.apply(m).getValue();
				return value != null ? FXCollections.singletonObservableList(value) : FXCollections.emptyObservableList();
			}

		};
		return forEach(applyOnModelList);
	}

	public static <N, M extends CompositeModel> MetaBinding<N, ObservableList<CompositeModel>> selector(Function<M, Property<CompositeModel>> applyOnModel, StringExtractor stringExtractor, Supplier<Generic> generic,
			ModelConstructor<CompositeModel> constructor) {

		Function<M, ObservableList<CompositeModel>> applyOnModelList = model -> new ListBinding<CompositeModel>() {
			{
				Property<CompositeModel> value = applyOnModel.apply(model);
				value.setValue(constructor.build(CompositeModel.addToGenerics(generic.get(), model.getGenerics()), stringExtractor));
				bind(value);
			}

			@Override
			protected ObservableList<CompositeModel> computeValue() {
				CompositeModel value = applyOnModel.apply(model).getValue();
				return value != null ? FXCollections.singletonObservableList(value) : FXCollections.emptyObservableList();
			}

		};
		return forEach(applyOnModelList);
	}
}
