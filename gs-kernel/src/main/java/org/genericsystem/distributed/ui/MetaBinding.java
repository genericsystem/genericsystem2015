package org.genericsystem.distributed.ui;

import java.util.function.Function;

import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.distributed.ui.models.CompositeModel;
import org.genericsystem.distributed.ui.models.CompositeModel.ModelConstructor;
import org.genericsystem.distributed.ui.models.CompositeModel.ObservableListExtractor;
import org.genericsystem.distributed.ui.models.CompositeModel.StringExtractor;

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

	public static <N, M extends Model, SUBMODEL extends Model> MetaBinding<N, ObservableList<SUBMODEL>> forEach(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<SUBMODEL> constructor) {
		return new MetaBinding<>(model -> ((CompositeModel<SUBMODEL>) model).getSubModels(), MetaBinder.<N, SUBMODEL> foreachBinder(stringExtractor, observableListExtractor, constructor));
	}

	// TODO clean this
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

}
