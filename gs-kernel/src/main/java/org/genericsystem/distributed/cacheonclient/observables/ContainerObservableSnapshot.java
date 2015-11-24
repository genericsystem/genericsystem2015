package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;

import org.genericsystem.common.PseudoConcurrentCollection;

public class ContainerObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

	PseudoConcurrentCollection<E> container = new PseudoConcurrentCollection<>();

	@Override
	public E get(int index) {
		return container.getByIndex(index);
	}

	public E get(Object o) {
		return container.get(o);
	}

	@Override
	public Iterator<E> iterator() {
		Iterator<E> containerIt = container.iterator();
		return new Iterator<E>() {

			private E next;

			@Override
			public boolean hasNext() {
				return containerIt.hasNext();
			}

			@Override
			public E next() {
				return next = containerIt.next();
			}

			@Override
			public void remove() {
				containerIt.remove();
				callObservers(new SimpleRemoveChange(next));
			}
		};
	}

	@Override
	public int size() {
		return container.size();
	}

	@Override
	public boolean add(E element) {
		container.add(element);
		callObservers(new SimpleAddChange(element));
		System.out.println("Add element in snapshot container");
		return true;
	}

	@Override
	public boolean remove(Object element) {
		boolean result = container.remove((E) element);
		if (result) {
			callObservers(new SimpleRemoveChange((E) element));
			System.out.println("Remove element in snapshot container");
		}
		return result;
	}

}