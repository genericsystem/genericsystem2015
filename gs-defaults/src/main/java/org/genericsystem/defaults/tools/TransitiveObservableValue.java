package org.genericsystem.defaults.tools;

import javafx.beans.InvalidationListener;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public abstract class TransitiveObservableValue<T> extends ListBinding<T> {

	private ObservableList<T> slave = FXCollections.emptyObservableList();
	protected final ObservableValue<T> master;
	@SuppressWarnings("unused")
	private final InvalidationListener listener;

	public TransitiveObservableValue(ObservableValue<T> master) {
		this.master = master;
		this.master.addListener(listener = l -> onMasterInvalidation());
		onMasterInvalidation();
	}

	public void changeSlave(ObservableList<T> observable) {
		unbind(slave);
		slave = observable;
		bind(observable);
	}

	protected abstract void onMasterInvalidation();

	protected ObservableList<T> getSlave() {
		return slave;
	}

}
