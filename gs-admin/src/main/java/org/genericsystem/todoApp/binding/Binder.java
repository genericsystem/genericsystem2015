package org.genericsystem.todoApp.binding;


public interface Binder<T> {
	public void init(T val, BindingContext context);
}
