package org.genericsystem.distributed.cacheonclient.observables;

import java.util.function.Predicate;

import javafx.collections.ObservableList;
import javafx.collections.ObservableSet;

public interface ObservableSnapshot<T> extends ObservableSet<T> {

	T get(int index);

	ObservableSnapshot<T> filtered(Predicate<T> predicate);

	ObservableSnapshot<T> concat(ObservableSnapshot<T> toConcatenate);

	ObservableList<T> toObservableList();
}
