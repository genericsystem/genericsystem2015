package org.genericsystem.distributed.cacheonclient.observables;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class ListWrapperContainerObservableSnapshot<E> extends AbstractObservableSnapshot<E> {

	private List<E> list = new ArrayList<>();

	public ListWrapperContainerObservableSnapshot(List<E> list) {
		this.list = list;
	}

	@Override
	public E get(int index) {
		return list.get(index);
	}

	@Override
	public Iterator<E> iterator() {
		return list.iterator();
	}

	@Override
	public int size() {
		return list.size();
	}
}