package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableListBase;

public class UnmodifiableList<T> extends ObservableListBase<T> implements Wrappable<T> {

	private final Wrappable<T> backingObject;

	public UnmodifiableList(Wrappable<T> backingList) {
		this.backingObject = backingList;
	}

	@Override
	public final int size() {
		return backingObject.size();
	}

	@Override
	public final T get(int index) {
		return backingObject.get(index);
	}

}
