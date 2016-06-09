package org.genericsystem.reactor;

import java.util.function.Function;

import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <T>
 */
public class MetaBinding<T extends ObservableList<?>> {

	private final Function<Model, T> applyOnModel;
	private final MetaBinder<T> binder;

	public MetaBinding(Function<Model, T> applyOnModel, MetaBinder<T> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ViewContext<?> viewContext, Element<?> childElement) {
		binder.init(applyOnModel, viewContext, childElement);
	}

	@Override
	protected void finalize() throws Throwable {
		System.out.println("What a surprize => could you warn Nicolas ?");
	};

	public static <M extends Model, SUBMODEL extends Model> MetaBinding<ObservableList<SUBMODEL>> forEach(Function<M, ObservableList<SUBMODEL>> applyOnModel) {
		return new MetaBinding<>(model -> applyOnModel.apply((M) model), MetaBinder.<SUBMODEL> foreachBinder());
	}
}
