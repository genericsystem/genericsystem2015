package org.genericsystem.distributed.cacheonclient.observables;

import java.util.concurrent.CompletableFuture;
import java.util.function.Function;

import javafx.beans.binding.SetBinding;
import javafx.collections.ObservableSet;

import org.genericsystem.common.Vertex;

public class CompletableObservableSnapshot2<E> extends SetBinding<E> {

	private final CompletableListObservableValue<E> observable;

	public CompletableObservableSnapshot2(CompletableFuture<Vertex[]> promise, Function<Vertex, E> extractor) {
		this.observable = new CompletableListObservableValue<>(extractor);
		bind(observable);
		observable.launch(promise);
	}

	@Override
	protected ObservableSet<E> computeValue() {
		return new ListWrapperContainerObservableSnapshot<>(observable.getValue());
	}
}