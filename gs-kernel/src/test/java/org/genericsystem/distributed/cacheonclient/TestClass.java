package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ListChangeListener;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.ObservableListBase;

public class TestClass<T> extends ObservableListBase<T> {
	private ObservableList<T> list;

	public TestClass(ObservableList<T> list) {
		this.list = list;
		list.addListener((ListChangeListener<? super T>) (onChange -> {
			System.out.println("before sourceChanged");
			sourceChanged(onChange);
			System.out.println("after sourceChanged");
		}));
	}

	protected void sourceChanged(Change<? extends T> c) {
		beginChange();
		System.out.println("in sourceChanged :: begin");
		while (c.next()) {
			System.out.println("in sourceChanged :: change :: next");
			if (c.wasPermutated()) {
				throw new UnsupportedOperationException();
			} else if (c.wasUpdated()) {
				throw new UnsupportedOperationException();
			} else {
				addRemove(c);
			}
		}
		System.out.println("in sourceChanged :: end");
		endChange();
	}

	private void addRemove(Change<? extends T> c) {
		System.out.println("in addremove");
		if (c.wasPermutated()) {
			throw new UnsupportedOperationException();
		} else {
			if (c.wasRemoved()) {
				System.out.println("in sourceChanged :: change :: remove");
				nextRemove(c.getFrom(), c.getRemoved());
			}
			if (c.wasAdded()) {
				System.out.println("in sourceChanged :: change :: add");
				nextAdd(c.getFrom(), c.getFrom() + c.getAddedSubList().size());
			}
		}

	}

	@Override
	public int size() {
		return list.size();
	}

	@Override
	public T get(int index) {
		return list.get(index);
	}

}
