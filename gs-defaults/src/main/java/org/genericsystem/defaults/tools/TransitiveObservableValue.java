package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.beans.value.ObservableValue;

//public abstract class TransitiveObservableValue<T> extends ListBinding<T> {
//
//	protected final ObservableListConcat<T> slave = new ObservableListConcat<>();
//	protected final ObservableValue<T> master;
//
//	public TransitiveObservableValue(ObservableValue<T> master) {
//		this.master = master;
//		bind(this.master);
//	}
//
//	public void bindSlave(ObservableList<T> observable) {
//		slave.addAllAndBind(observable);
//	}
//
//	protected void unbindAllSlaves() {
//		clear();
//	}
//
//}
public abstract class TransitiveObservableValue<T> extends ListBinding<T> {

	private final List<Observable> slaves = new ArrayList<>();
	protected final ObservableValue<T> master;

	public TransitiveObservableValue(ObservableValue<T> master) {
		this.master = master;
		bind(this.master);
	}

	public void bindSlave(Observable observable) {
		slaves.add(observable);
		bind(observable);
	}

	protected void unbindAllSlaves() {
		for (Observable slave : slaves)
			unbind(slave);
		slaves.clear();
	}

}
