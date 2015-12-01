package org.genericsystem.distributed.cacheonclient;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.value.ObservableValueBase;

class Invalidator<T> extends ObservableValueBase<T> {

	private InvalidationListener listener = o -> {
		super.fireValueChangedEvent();
	};

	public static <T> Invalidator<T> createInvalidator(Observable... observables) {
		return new Invalidator<T>(observables);
	}

	private final Observable[] observables;

	private Invalidator(Observable... observables) {
		this.observables = observables;
		for (Observable observable : observables)
			observable.addListener(new WeakInvalidationListener(listener));
	}

	// @Override
	// public void invalidated(Observable observable) {
	// System.out.println("Invidation in concatinvalidator");
	// super.fireValueChangedEvent();
	// }

	@Override
	public T getValue() {
		return null;
	}

}