//package org.genericsystem.distributed.cacheonclient.observables;
//
//import java.util.ArrayList;
//import java.util.Arrays;
//import java.util.Iterator;
//import java.util.List;
//import java.util.concurrent.CompletableFuture;
//import java.util.function.Function;
//import java.util.stream.Collectors;
//
//import org.genericsystem.common.Vertex;
//
//public class CompletableObservableSnapshot<E> extends AbstractObservableSnapshot<E> {
//
//	private List<E> list = new ArrayList<>();
//
//	public CompletableObservableSnapshot(CompletableFuture<Vertex[]> promise, Function<Vertex, E> extractor) {
//		promise.thenAccept(elements -> {
//			list = Arrays.stream(elements).map(vertex -> extractor.apply(vertex)).collect(Collectors.toList());
//			list.forEach(element -> callObservers(new SimpleAddChange(element)));
//		});
//	}
//
//	@Override
//	public E get(int index) {
//		return list.get(index);
//	}
//
//	@Override
//	public Iterator<E> iterator() {
//		return list.iterator();
//	}
//
//	@Override
//	public int size() {
//		return list.size();
//	}
// }