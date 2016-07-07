package org.genericsystem.remote;

import javafx.collections.ListChangeListener;
import javafx.collections.ListChangeListener.Change;
import javafx.collections.ObservableList;
import javafx.collections.ObservableListBase;

public class TestClass<T> extends ObservableListBase<T> {
	private ObservableList<T> list;

	public TestClass(ObservableList<T> list) {
		this.list = list;
		list.addListener((ListChangeListener<? super T>) (onChange -> {
			sourceChanged(onChange);
		}));
	}

	protected void sourceChanged(Change<? extends T> c) {
		beginChange();
		while (c.next()) {
			if (c.wasPermutated()) {
				throw new UnsupportedOperationException();
			} else if (c.wasUpdated()) {
				throw new UnsupportedOperationException();
			} else {
				addRemove(c);
			}
		}
		endChange();
	}

	private void addRemove(Change<? extends T> c) {
		if (c.wasPermutated()) {
			throw new UnsupportedOperationException();
		} else {
			if (c.wasRemoved()) {
				nextRemove(c.getFrom(), c.getRemoved());
			}
			if (c.wasAdded()) {
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
