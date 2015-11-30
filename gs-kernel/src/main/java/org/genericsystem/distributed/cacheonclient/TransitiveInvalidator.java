package org.genericsystem.distributed.cacheonclient;

import java.util.function.Supplier;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.ObservableValueBase;

class TransitiveInvalidator<T> extends ObservableValueBase<T> {

	private Observable observableMaster;

	private Observable observableSlave;
	Supplier<Observable> slaveObservableExtractor;

	public static <T> TransitiveInvalidator<T> create(ObservableValue<T> observableMaster, Supplier<Observable> slaveObservableExtractor) {
		return new TransitiveInvalidator<>(observableMaster, slaveObservableExtractor);
	}

	private InvalidationListener slaveInvalidationListener = o -> {
		System.out.println("transitive slave invalidation");
		fireValueChangedEvent();
	};

	private InvalidationListener masterInvalidationListener = o -> {
		System.out.println("transitive master invalidation");
		observableSlave.removeListener(slaveInvalidationListener);
		observableSlave = slaveObservableExtractor.get();
		observableSlave.addListener(slaveInvalidationListener);
		fireValueChangedEvent();
	};

	public TransitiveInvalidator(ObservableValue<T> observableMaster, Supplier<Observable> slaveObservableExtractor) {
		this.observableMaster = observableMaster;
		this.slaveObservableExtractor = slaveObservableExtractor;

		observableMaster.addListener(new WeakInvalidationListener(masterInvalidationListener));
		observableSlave = slaveObservableExtractor.get();
		observableSlave.addListener(new WeakInvalidationListener(slaveInvalidationListener));
	}

	@Override
	protected void finalize() throws Throwable {
		System.out.println("finalize CCC");
	}

	@Override
	public T getValue() {
		return null;
	}

}