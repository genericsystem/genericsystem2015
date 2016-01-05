package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.InvalidationListener;
import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.ObservableList;

public abstract class TransitiveObservableList<T> extends ListBinding<T> {

	private final List<ObservableList<T>> slaves = new ArrayList<>();
	protected final ObservableList<T> master;
	@SuppressWarnings("unused")
	private final InvalidationListener listener;

	public TransitiveObservableList(ObservableList<T> master) {
		this.master = master;
		this.master.addListener(listener = l -> onMasterInvalidation());
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
