package org.genericsystem.distributed.ui;

import java.util.function.Function;

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
public class MetaBinding<N, T> {

	private final Function<Model, T> applyOnModel;
	private final MetaBinder<N, T> binder;

	public MetaBinding(Function<Model, T> applyOnModel, MetaBinder<N, T> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ViewContext<?, N> viewContext, Element<?, ?> childElement) {
		binder.init(applyOnModel, viewContext, childElement);
	}

	static <N, M, W> MetaBinding<N, W> bind(Function<M, W> applyOnModel, MetaBinder<N, W> binder) {
		return new MetaBinding<>((u) -> applyOnModel.apply((M) u), binder);
	}

	public static <N, M extends Model, T extends Model> MetaBinding<N, ObservableList<T>> forEach(Function<M, ObservableList<T>> applyOnModel) {
		return bind(applyOnModel, MetaBinder.<N, T> foreachBinder());
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

}
