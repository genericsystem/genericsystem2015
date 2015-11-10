package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;

public interface Wrappable<T> extends ObservableList<T> {

	@Override
	public int size();

	@Override
	public T get(int index);

}
