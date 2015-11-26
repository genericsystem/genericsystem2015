package org.genericsystem.distributed.cacheonclient;

import javafx.beans.Observable;
import javafx.beans.binding.ObjectBinding;
import javafx.beans.property.ObjectProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.common.IDifferential;

public abstract class GSObjectBinding<T extends Observable> extends ObjectBinding<T> {

	private T currentlyBinding;
	private final boolean bindT;

	public GSObjectBinding(T observable, ObjectProperty<? extends IDifferential<Generic>> property, boolean bindT) {
		this.bindT = bindT;
		currentlyBinding = observable;
		bind(property);
		if (bindT)
			bind(currentlyBinding);
	}

	@Override
	protected T computeValue() {
		return currentlyBinding;
	}

	@Override
	abstract protected void onInvalidating();

	/**
	 * Call changeBindedObject in onInvalidation.
	 * 
	 * @param t
	 *            new version of binded object
	 */
	final protected void changeBindedObject(T t) {
		if (bindT)
			unbind(currentlyBinding);

		currentlyBinding = t;

		if (bindT)
			bind(t);
	}

}
