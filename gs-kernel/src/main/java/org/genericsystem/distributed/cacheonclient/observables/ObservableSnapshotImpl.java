package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;

import javafx.collections.WeakSetChangeListener;

import com.sun.javafx.collections.SetAdapterChange;

public class ObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

	private final ObservableSnapshot<E> backingSet;

	public ObservableSnapshotImpl(ObservableSnapshot<E> set) {
		this.backingSet = set;
		this.backingSet.addListener(new WeakSetChangeListener<E>(c -> {
			callObservers(new SetAdapterChange<E>(ObservableSnapshotImpl.this, c));
		}));
	}

	@Override
	public int size() {
		return backingSet.size();
	}

	@Override
	public Iterator<E> iterator() {
		return backingSet.iterator();
	}

	@Override
	public E get(int index) {
		return backingSet.get(index);
	}

}