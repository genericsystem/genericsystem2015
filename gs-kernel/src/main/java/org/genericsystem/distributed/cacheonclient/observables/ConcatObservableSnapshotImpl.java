package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;
import java.util.stream.Stream;

import javafx.collections.SetChangeListener;
import javafx.collections.WeakSetChangeListener;

import com.sun.javafx.collections.SetAdapterChange;

public class ConcatObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

	private final ObservableSnapshot<E> backingSet;
	private final ObservableSnapshot<E> backingSet2;

	@SuppressWarnings("unused")
	private final SetChangeListener<E> listenerBackingSet1;
	@SuppressWarnings("unused")
	private final SetChangeListener<E> listenerBackingSet2;

	public ConcatObservableSnapshotImpl(ObservableSnapshot<E> backingSet, ObservableSnapshot<E> backingSet2) {
		this.backingSet = backingSet;
		this.backingSet2 = backingSet2;

		this.backingSet.addListener(new WeakSetChangeListener<E>(listenerBackingSet1 = (c -> {
			callObservers(new SetAdapterChange<E>(ConcatObservableSnapshotImpl.this, c));
		})));
		this.backingSet2.addListener(new WeakSetChangeListener<E>(listenerBackingSet2 = (c -> {
			callObservers(new SetAdapterChange<E>(ConcatObservableSnapshotImpl.this, c));
		})));
	}

	@Override
	public int size() {
		return backingSet.size() + backingSet2.size();
	}

	@Override
	public Iterator<E> iterator() {
		return Stream.concat(backingSet.stream(), backingSet2.stream()).iterator();
	}

	@Override
	public E get(int index) {
		return index < backingSet.size() ? backingSet.get(index) : backingSet2.get(index - backingSet.size());
	}
}