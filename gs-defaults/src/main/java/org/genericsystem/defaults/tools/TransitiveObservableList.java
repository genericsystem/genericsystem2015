package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class TransitiveObservableList<T> extends AbstractMinimalChangesObservableList<T> {

	private final List<Observable> slaves = new ArrayList<>();
	protected final ObservableList<T> master;
	private final InvalidationListener listener = l -> onMasterInvalidation();
	private final Function<T, Observable> observableSlaves;

	public TransitiveObservableList(ObservableList<T> master, Function<T, Observable> observableSlaves) {
		this.master = master;
		this.observableSlaves = observableSlaves;
		this.master.addListener(new WeakInvalidationListener(listener));
		onMasterInvalidation();
	}

	protected void bindSlave(Observable observable) {
		slaves.add(observable);
		bind(observable);
	}

	protected void onMasterInvalidation() {
		for (Observable slave : slaves)
			unbind(slave);
		slaves.clear();
		invalidate();
		for (T elt : master)
			bindSlave(observableSlaves.apply(elt));
	}
}
