package org.genericsystem.distributed.cacheonclient.observables;

import java.util.Iterator;

import org.genericsystem.common.PseudoConcurrentCollection;

public class ContainerObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

	PseudoConcurrentCollection<E> container = new PseudoConcurrentCollection<>();

	@Override
	public E get(int index) {
		return container.getByIndex(index);
	}

	@Override
	public Iterator<E> iterator() {
		return container.iterator();
	}

	@Override
	public int size() {
		return container.size();
	}

	@Override
	public boolean add(E element) {
		container.add(element);
		callObservers(new SimpleAddChange(element));
		return true;
	}

	@Override
	public boolean remove(Object element) {
		boolean result = container.remove((E) element);
		if (result)
			callObservers(new SimpleRemoveChange((E) element));
		return result;
	}

}