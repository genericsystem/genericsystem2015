//package org.genericsystem.distributed.cacheonclient.observables;
//
//import java.util.function.Predicate;
//
//import javafx.beans.value.ObservableValue;
//import javafx.collections.ObservableList;
//import javafx.collections.ObservableSet;
//
//public interface ObservableSnapshot<T> extends ObservableSet<T> {
//
//	T get(int index);
//
//	default ObservableSnapshot<T> filtered(Predicate<T> predicate) {
//		return new FilterObservableSnapshotImpl<>(this, predicate);
//	}
//
//	default ObservableSnapshot<T> filtered(ObservableValue<Predicate<T>> predicate) {
//		return new ObservableFilterObservableSnapshotImpl<>(this, predicate);
//	}
//
//	default ObservableSnapshot<T> concat(ObservableSnapshot<T> toConcatenate) {
//		return new ConcatObservableSnapshotImpl<>(this, toConcatenate);
//	}
//
//	default ObservableList<T> toObservableList() {
//		return new ObservableListSnapshot<>(this);
//	}
//
// }
