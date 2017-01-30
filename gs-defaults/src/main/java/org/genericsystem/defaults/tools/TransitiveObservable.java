//package org.genericsystem.defaults.tools;
//
//import java.util.ArrayList;
//import java.util.List;
//import java.util.function.Supplier;
//
//import javafx.beans.InvalidationListener;
//import javafx.beans.Observable;
//import javafx.beans.WeakInvalidationListener;
//
///**
// * @author Nicolas Feybesse
// *
// * @param <T>
// */
//public class TransitiveObservable extends ObservableBase {
//
//	private List<Observable> slaves = new ArrayList<>();
//	// private final Observable observableMaster;
//	private final Supplier<Observable[]> observableSlaves;
//	private InvalidationListener masterInvalidationListener = o -> onMasterInvalidation();
//
//	public static TransitiveObservable create(Observable observableMaster, Supplier<Observable[]> observablesSlaves) {
//		return new TransitiveObservable(observableMaster, observablesSlaves);
//	}
//
//	private TransitiveObservable(Observable observableMaster, Supplier<Observable[]> observableSlaves) {
//		super();
//		// this.observableMaster = observableMaster;
//		this.observableSlaves = observableSlaves;
//		observableMaster.addListener(new WeakInvalidationListener(masterInvalidationListener));
//		for (Observable o : observableSlaves.get())
//			bindSlave(o);
//	}
//
//	protected void bindSlave(Observable observable) {
//		slaves.add(observable);
//		bind(observable);
//	}
//
//	protected void onMasterInvalidation() {
//		for (Observable slave : slaves)
//			unbind(slave);
//		slaves.clear();
//		invalidate();
//		for (Observable o : observableSlaves.get())
//			bindSlave(o);
//	}
// }