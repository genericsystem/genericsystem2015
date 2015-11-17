package org.genericsystem.distributed.cacheonclient;

import javafx.collections.ObservableList;
import javafx.collections.ObservableListBase;

public abstract class AbstractWrappable<T> extends ObservableListBase<T> implements Wrappable<T> {

	static class WrappableImpl<T> extends AbstractWrappable<T> {
		protected ObservableList<T> backingObject;

		public WrappableImpl(Wrappable<T> backingObject) {
			this.backingObject = backingObject;
		}

		@Override
		public final int size() {
			return backingObject.size();
		}

		@Override
		public final T get(int index) {
			return backingObject.get(index);
		}
	}
}
