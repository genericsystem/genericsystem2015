package org.genericsystem.distributed.cacheonclient.observables;

import java.util.AbstractSet;

import javafx.beans.InvalidationListener;
import javafx.collections.SetChangeListener;

import com.sun.javafx.collections.SetListenerHelper;

@SuppressWarnings({ "restriction" })
public abstract class AbstractObservableSnapshot<E> extends AbstractSet<E> implements ObservableSnapshot<E> {

	private SetListenerHelper<E> listenerHelper;

	protected void callObservers(SetChangeListener.Change<E> change) {
		SetListenerHelper.fireValueChangedEvent(listenerHelper, change);
	}

	@Override
	public void addListener(InvalidationListener listener) {
		listenerHelper = SetListenerHelper.addListener(listenerHelper, listener);
	}

	@Override
	public void removeListener(InvalidationListener listener) {
		listenerHelper = SetListenerHelper.removeListener(listenerHelper, listener);
	}

	@Override
	public void addListener(SetChangeListener<? super E> observer) {
		listenerHelper = SetListenerHelper.addListener(listenerHelper, observer);
	}

	@Override
	public void removeListener(SetChangeListener<? super E> observer) {
		listenerHelper = SetListenerHelper.removeListener(listenerHelper, observer);
	}

	class SimpleAddChange extends SetChangeListener.Change<E> {

		private final E added;

		public SimpleAddChange(E added) {
			super(AbstractObservableSnapshot.this);
			this.added = added;
		}

		@Override
		public boolean wasAdded() {
			return true;
		}

		@Override
		public boolean wasRemoved() {
			return false;
		}

		@Override
		public E getElementAdded() {
			return added;
		}

		@Override
		public E getElementRemoved() {
			return null;
		}

		@Override
		public String toString() {
			return "added " + added;
		}

	}

	class SimpleRemoveChange extends SetChangeListener.Change<E> {

		private final E removed;

		public SimpleRemoveChange(E removed) {
			super(AbstractObservableSnapshot.this);
			this.removed = removed;
		}

		@Override
		public boolean wasAdded() {
			return false;
		}

		@Override
		public boolean wasRemoved() {
			return true;
		}

		@Override
		public E getElementAdded() {
			return null;
		}

		@Override
		public E getElementRemoved() {
			return removed;
		}

		@Override
		public String toString() {
			return "removed " + removed;
		}

	}

}