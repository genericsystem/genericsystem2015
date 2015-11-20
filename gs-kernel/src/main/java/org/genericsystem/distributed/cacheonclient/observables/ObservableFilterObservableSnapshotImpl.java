package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;
import java.util.function.Predicate;

import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.collections.SetChangeListener;
import javafx.collections.WeakSetChangeListener;

import com.sun.javafx.collections.SetAdapterChange;

public class ObservableFilterObservableSnapshotImpl<E> extends AbstractObservableSnapshot<E> {

	private final ObservableSnapshot<E> backingSet;
	private final ObservableValue<Predicate<E>> predicate;
	private int filteredSize;
	@SuppressWarnings("unused")
	private final SetChangeListener<E> listener;

	@SuppressWarnings("unused")
	private final ChangeListener<Predicate<E>> addListener;

	@SuppressWarnings("restriction")
	public ObservableFilterObservableSnapshotImpl(ObservableSnapshot<E> set, ObservableValue<Predicate<E>> predicate) {
		this.backingSet = set;
		this.predicate = predicate;
		predicate.addListener(addListener = (ChangeListener<Predicate<E>>) (o, oldPredicate, newPredicate) -> {
			backingSet.stream().forEach(g -> {
				if (oldPredicate.test(g)) {
					callObservers(new SimpleRemoveChange(g));
				}

				if (newPredicate.test(g)) {
					callObservers(new SimpleAddChange(g));
				}
			});

			filteredSize = Long.valueOf(backingSet.stream().filter(newPredicate).count()).intValue();

			// Fastest way, but do not support less restrictive filter than previous (lose order)
			// backingSet.stream().forEach(g -> {
			// boolean newSelected = newPredicate.test(g);
			// if (oldPredicate.test(g) != newSelected) {
			// if (newSelected)
			// callObservers(new SimpleAddChange(g));
			// else
			// callObservers(new SimpleRemoveChange(g));
			// }
			// });

		});
		this.filteredSize = Long.valueOf(backingSet.stream().filter(predicate.getValue()).count()).intValue();
		this.backingSet.addListener(new WeakSetChangeListener<E>(listener = (c -> {
			if (c.wasAdded() && predicate.getValue().test(c.getElementAdded())) {
				filteredSize++;
				callObservers(new SetAdapterChange<E>(ObservableFilterObservableSnapshotImpl.this, c));
			} else if (c.wasRemoved() && predicate.getValue().test(c.getElementRemoved())) {
				filteredSize--;
				callObservers(new SetAdapterChange<E>(ObservableFilterObservableSnapshotImpl.this, c));
			}
		})));
	}

	@Override
	public int size() {
		// assert filteredSize == Long.valueOf(backingSet.stream().filter(predicate.getValue()).count()).intValue();
		return filteredSize;
	}

	@Override
	public Iterator<E> iterator() {
		return backingSet.stream().filter(predicate.getValue()).iterator();
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