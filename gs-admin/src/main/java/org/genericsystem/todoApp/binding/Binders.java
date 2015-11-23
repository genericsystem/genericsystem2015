package org.genericsystem.todoApp.binding;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.Label;

import org.genericsystem.todoApp.IModelContext;
import org.genericsystem.todoApp.IModelContext.ModelContextImpl;
import org.genericsystem.todoApp.IViewContext.AbstractViewContext;

public abstract class Binders<Type> {

	public static class RemoveBinder<T> {
		public static <T> Binder<T> removeBind() {
			Binder<T> imp = new Binder<T>() {

				@Override
				public void init(ObservableValue<T> val, BindingContext context) {

				}

				@Override
				public Consumer<T> notifyImpl(BindingContext context) {
					return null;
				}

			};
			return null;
		}
	}

	public static class TodoBinder<T> {
		public static <T> Binder<T> todoBind() {
			Binder<T> imp = new Binder<T>() {
				@Override
				public void init(ObservableValue<T> val, BindingContext context) {
					((Label) (((AbstractViewContext) context.viewContext).node)).textProperty().set(val.getValue().toString());
				}

				@Override
				public Consumer<T> notifyImpl(BindingContext context) {
					return null;
				}
			};
			return imp;
		}
	}

	public static class foreachBinder<T> {

		public static <T> Binder<List<T>> foreach() {

			Binder<List<T>> impl = new Binder<List<T>>() {
				List<IModelContext> contexts = new LinkedList<>();
				List<T> values = new LinkedList<>();

				@Override
				public Consumer<List<T>> notifyImpl(BindingContext context) {
					return myList -> {
						List<T> listDiff = diff(values, myList);
						listDiff.forEach(t -> {
							ModelContextImpl childContext = (ModelContextImpl) context.modelContext.createChild(t);
							context.viewContext.bind(childContext);
							values.add(t);
							contexts.add(childContext);
						});
					};
				}

				public List<T> diff(List<T> values, List<T> myList) {
					List<T> diff = new ArrayList<T>();
					return myList;
				}

				@Override
				public void init(ObservableValue<List<T>> val, BindingContext context) {
					((AbstractViewContext) context.viewContext).initContent = false;
					Consumer<List<T>> notifyImpl = notifyImpl(context);
					notifyImpl.accept(val.getValue());
				}
			};
			return impl;
		}
	}
}

//
// Impl impl = new Impl();