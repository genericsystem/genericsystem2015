package org.genericsystem.ui;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

public class MetaBinding<N, T> {

	private final Function<?, T> method;
	private final MetaBinder<N, T> binder;

	public MetaBinding(MetaBinder<N, T> binder, Function<?, T> method) {
		this.binder = binder;
		this.method = method;
	}

	public void init(ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
		binder.init(method, modelContext, viewContext, childElement);
	}

	static <N, M, T> MetaBinding<N, T> bind(MetaBinder<N, T> binder, Function<M, T> function) {
		return new MetaBinding<>(binder, (u) -> function.apply((M) u));
	}

	public static <N, M, T> MetaBinding<N, ObservableList<T>> forEach(Function<M, ObservableList<T>> function) {
		return MetaBinding.bind(MetaBinder.foreachBinder(), function);
	}

	public static <N, M, T> MetaBinding<N, ObservableValue<T>> selector(Function<M, ObservableValue<T>> function) {
		return MetaBinding.bind(MetaBinder.selectorBinder(), function);
	}
}
