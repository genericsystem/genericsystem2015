//package org.genericsystem.distributed.ui.utils;
//
//import java.util.function.Function;
//
//import javafx.beans.binding.ObjectBinding;
//import javafx.beans.value.ObservableValue;
//
//public class OneShotBindings<T, S> extends ObjectBinding<S> {
//
//	private final ObservableValue<T> observable;
//	private final Function<T, S> function;
//
//	public OneShotBindings(ObservableValue<T> observable, Function<T, S> function) {
//		this.observable = observable;
//		this.function = function;
//		bind(observable);
//	}
//
//	static public <T, S> OneShotBindings<T, S> createInitializer(ObservableValue<T> observable, Function<T, S> function) {
//		return new OneShotBindings<>(observable, function);
//	}
//
//	@Override
//	protected void onInvalidating() {
//		unbind(observable);
//	};
//
//	@Override
//	protected S computeValue() {
//		return function.apply(observable.getValue());
//	}
// }