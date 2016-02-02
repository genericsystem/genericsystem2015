package org.genericsystem.distributed.cacheonclient;

import java.util.function.Supplier;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.ObservableValueBase;

class TransitiveInvalidator<T> extends ObservableValueBase<T> {

	private Observable observableSlave;
	private Supplier<Observable> slaveObservableExtractor;
	private InvalidationListener slaveInvalidationListener = o -> fireValueChangedEvent();
	private InvalidationListener masterInvalidationListener = o -> {
		System.out.println("TransitiveInvalidator :: masterInvalidationListener");
		observableSlave.removeListener(slaveInvalidationListener);
		observableSlave = slaveObservableExtractor.get();
		observableSlave.addListener(slaveInvalidationListener);
		fireValueChangedEvent();
	};

	public static <T> TransitiveInvalidator<T> create(ObservableValue<T> observableMaster, Supplier<Observable> slaveObservableExtractor) {
		return new TransitiveInvalidator<>(observableMaster, slaveObservableExtractor);
	}

	private TransitiveInvalidator(ObservableValue<T> observableMaster, Supplier<Observable> slaveObservableExtractor) {
		this.slaveObservableExtractor = slaveObservableExtractor;

		observableMaster.addListener(new WeakInvalidationListener(masterInvalidationListener));
		observableSlave = slaveObservableExtractor.get();
		observableSlave.addListener(new WeakInvalidationListener(slaveInvalidationListener));
	}

	@Override
	public T getValue() {
		return null;
	}

}