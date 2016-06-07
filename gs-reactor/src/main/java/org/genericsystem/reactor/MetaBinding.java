package org.genericsystem.reactor;

import java.util.function.Function;

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
		System.out.println("What a surprize => could you warn Nicolas ?");
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

}
