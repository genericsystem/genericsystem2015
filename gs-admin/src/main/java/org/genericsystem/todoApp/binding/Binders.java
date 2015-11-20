package org.genericsystem.todoApp.binding;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;

import javafx.beans.value.ObservableValue;

import org.genericsystem.todoApp.IModelContext;
import org.genericsystem.todoApp.IModelContext.ModelContextImpl;

public abstract class Binders<Type> {

	public static class foreachBinder<T> {

		public static <T> Binder<List<T>> foreach() {

			Binder<List<T>> impl = new Binder<List<T>>() {
				List<IModelContext> contexts = new LinkedList<>();
				List<T> values = new LinkedList<>();

				// List<T> myList = new LinkedList<T>();// la liste passée en param (où ??)

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
					System.out.println("init foreach :: val " + val.getValue());

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