package org.genericsystem.distributed.cacheonclient.InvalidationTools;

import java.lang.ref.WeakReference;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;

public class GSObservable implements Observable {

	private Observable observable = null;
	private InvalidationListener listener = null;
	private GSExpressionHelper helper = null;

	public GSObservable() {
	}

	@Override
	public void addListener(InvalidationListener listener) {
		helper = GSExpressionHelper.addListener(helper, this, listener);

	}

	@Override
	public void removeListener(InvalidationListener listener) {
		helper = GSExpressionHelper.removeListener(helper, listener);
	}

	/**
	 * Sends notifications to all attached {@link javafx.beans.InvalidationListener InvalidationListeners} and {@link javafx.beans.value.ChangeListener ChangeListeners}.
	 *
	 * This method is called when the value is changed, either manually by calling {@link #set} or in case of a bound property, if the binding becomes invalid.
	 */
	public void fireValueChangedEvent() {
		GSExpressionHelper.fireValueChangedEvent(helper);
	}

	public void bind(final Observable newObservable) {
		if (newObservable == null) {
			throw new NullPointerException("Cannot bind to null");
		}

		if (!newObservable.equals(this.observable)) {
			unbind();
			observable = newObservable;
			if (listener == null) {
				listener = new Listener(this);
			}
			observable.addListener(listener);
			fireValueChangedEvent();
		}
	}

	public void unbind() {
		if (observable != null) {
			observable.removeListener(listener);
			observable = null;
		}
	}

	private static class Listener implements InvalidationListener {

		private final WeakReference<GSObservable> wref;

		public Listener(GSObservable ref) {
			this.wref = new WeakReference<GSObservable>(ref);
		}

		@Override
		public void invalidated(Observable observable) {
			GSObservable ref = wref.get();
			if (ref == null) {
				observable.removeListener(this);
			} else {
				ref.fireValueChangedEvent();
			}
		}
	}
}
