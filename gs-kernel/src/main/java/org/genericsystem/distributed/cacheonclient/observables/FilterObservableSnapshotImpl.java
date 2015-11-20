package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;
import java.util.function.Predicate;

import javafx.collections.SetChangeListener;
import javafx.collections.WeakSetChangeListener;

import com.sun.javafx.collections.SetAdapterChange;

public class FilterObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

	private final ObservableSnapshot<E> backingSet;
	private final Predicate<E> predicate;
	private final SetChangeListener<E> listener;
	private int filteredSize;

	public FilterObservableSnapshotImpl(ObservableSnapshot<E> set, Predicate<E> predicate) {
		this.backingSet = set;
		this.predicate = predicate;
		this.filteredSize = Long.valueOf(backingSet.stream().filter(predicate).count()).intValue();

		this.backingSet.addListener(new WeakSetChangeListener<E>(listener = (c -> {
			if (c.wasAdded() && predicate.test(c.getElementAdded())) {
				filteredSize++;
				callObservers(new SetAdapterChange<E>(FilterObservableSnapshotImpl.this, c));
			} else if (c.wasRemoved() && predicate.test(c.getElementRemoved())) {
				filteredSize--;
				callObservers(new SetAdapterChange<E>(FilterObservableSnapshotImpl.this, c));
			}
		})));
	}

	@Override
	public int size() {
		// assert filteredSize == Long.valueOf(backingSet.stream().filter(predicate).count()).intValue();
		return filteredSize;
	}

	@Override
	public Iterator<E> iterator() {
		return backingSet.stream().filter(predicate).iterator();
	}

	@Override
	public E get(int index) {
		// TODO KK
		Iterator<E> iterator = iterator();
		int i = 0;
		while (iterator.hasNext()) {
			if (index == i)
				return iterator.next();
			iterator.next();
			i++;
		}
		return null;
	}

}