package org.genericsystem.distributed.cacheonclient.observables;

import javafx.collections.ObservableListBase;

public class ObservableListSnapshot<E> extends ObservableListBase<E> {
	private final ObservableSnapshot<E> snapshot;

	public ObservableListSnapshot(ObservableSnapshot<E> snapshot) {
		this.snapshot = snapshot;
	}

	@Override
	public E get(int index) {
		return snapshot.get(index);
	}

	@Override
	public int size() {
		return snapshot.size();
	}
}