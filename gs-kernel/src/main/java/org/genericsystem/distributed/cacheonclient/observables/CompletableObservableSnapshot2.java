//package org.genericsystem.distributed.cacheonclient.observables;
//
//import java.util.concurrent.CompletableFuture;
//import java.util.function.Function;
//
//import javafx.beans.binding.SetBinding;
//import javafx.collections.ObservableList;
//import javafx.collections.ObservableSet;
//
//import org.genericsystem.common.Vertex;
//
//public class CompletableObservableSnapshot2<E> extends SetBinding<E> implements ObservableSnapshot<E> {
//
//	private final CompletableListObservableValue<E> observable;
//
//	public CompletableObservableSnapshot2(CompletableFuture<Vertex[]> promise, Function<Vertex, E> extractor) {
//		observable = new CompletableListObservableValue<>(extractor);
//		bind(observable);
//		get();
//		observable.launch(promise);
//
//	}
//
//	@Override
//	protected ObservableSet<E> computeValue() {
//		return new ListWrapperContainerObservableSnapshot<>(observable.getValue());
//	}
//
//	@Override
//	public E get(int index) {
//		return ((ObservableSnapshot<E>) get()).get(index);
//	}
//
//	@Override
//	public ObservableList<E> toObservableList() {
//		return ((ObservableSnapshot<E>) get()).toObservableList();
//	}
// }