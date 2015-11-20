package org.genericsystem.todoApp.binding;

import java.util.function.Consumer;

import javafx.beans.value.ObservableValue;

public interface Binder<T> {
	public void init(ObservableValue<T> val, BindingContext context);

	public Consumer<T> notifyImpl(BindingContext context);
}
