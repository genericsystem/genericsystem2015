package org.genericsystem.defaults.tools;

import java.util.ArrayList;
import java.util.List;

import javafx.beans.Observable;
import javafx.beans.binding.ListBinding;
import javafx.collections.ObservableList;

//public abstract class TransitiveObservableList<T> extends ListBinding<T> {
//
//	private final ObservableListConcat<T> slave = new ObservableListConcat<>();
//	protected final ObservableList<T> master;
//
//	public TransitiveObservableList(ObservableList<T> master) {
//		this.master = master;
//		bind(this.master);
//		this.master.addListener((InvalidationListener) l -> System.out.println("_master invalidation"));
//	}
//
//	public void bindSlave(ObservableList<T> observable) {
//		slave.addAllAndBind(observable);
//	}
//
//	protected void unbindAllSlaves() {
//		slave.clear();
//	}
//}

public abstract class TransitiveObservableList<T> extends ListBinding<T> {

	private final List<Observable> slaves = new ArrayList<>();
	protected final ObservableList<T> master;

	public TransitiveObservableList(ObservableList<T> master) {
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
