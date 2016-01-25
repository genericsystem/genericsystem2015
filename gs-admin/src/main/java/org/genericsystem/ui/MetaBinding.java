package org.genericsystem.ui;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public class MetaBinding<N, T> {

	private final Function<?, T> applyOnModel;
	private final MetaBinder<N, T> binder;

	public MetaBinding(Function<?, T> applyOnModel, MetaBinder<N, T> binder) {
		this.applyOnModel = applyOnModel;
		this.binder = binder;
	}

	public void init(ViewContext<N> viewContext, Element<?> childElement) {
		binder.init(applyOnModel, viewContext, childElement);
	}

	static <N, M, T> MetaBinding<N, T> bind(Function<M, T> applyOnModel, MetaBinder<N, T> binder) {
		return new MetaBinding<>((u) -> applyOnModel.apply((M) u), binder);
	}

	public static <N, M, T> MetaBinding<N, ObservableList<T>> forEach(Function<M, ObservableList<T>> applyOnModel) {
		return MetaBinding.bind(applyOnModel, MetaBinder.foreachBinder());
	}

	public static <N, M, T> MetaBinding<N, ObservableValue<T>> selector(Function<M, ObservableValue<T>> applyOnModel) {
		return MetaBinding.bind(applyOnModel, MetaBinder.selectorBinder());
	}
}
