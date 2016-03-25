package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.WeakInvalidationListener;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 * @param <T>
 */
public abstract class TransitiveObservableList<T> extends MinimalChangesObservableList<T> {

	private final List<ObservableList<T>> slaves = new ArrayList<>();
	protected final ObservableList<T> master;
	private final InvalidationListener listener = l -> onMasterInvalidation();

	public TransitiveObservableList(ObservableList<T> master) {
		this.master = master;
		this.master.addListener(new WeakInvalidationListener(listener));
		onMasterInvalidation();
	}

	protected void bindSlave(ObservableList<T> observable) {
		slaves.add(observable);
		bind(observable);
	}

	protected abstract void onMasterInvalidation();

	protected List<ObservableList<T>> getSlaves() {
		return slaves;
	}

	protected void unbindAllSlaves() {
		for (Observable slave : slaves)
			unbind(slave);
		slaves.clear();
	}

}
