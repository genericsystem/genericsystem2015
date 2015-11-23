package org.genericsystem.todoApp.binding;

import java.lang.reflect.Field;

import javafx.beans.value.ObservableValue;

import org.genericsystem.todoApp.IModelContext.AbstractModelContext;

public interface Binding {

	public abstract void init(BindingContext context);

	public static class BindingImpl implements Binding {
		Field attribute;
		Binder binder;

		public BindingImpl bindTo(Field attribute, Binder binder) {
			this.attribute = attribute;
			this.binder = binder;
			return this;
		}

		@Override
		public void init(BindingContext context) {
			Object model;
			try {
				model = resolve(attribute, context);
				binder.init((ObservableValue) model, context);
				System.out.println("model :: " + model);
			} catch (IllegalArgumentException | IllegalAccessException e) {
				e.printStackTrace();
			}
		}

		private Object resolve(Field attribute, BindingContext context) throws IllegalArgumentException, IllegalAccessException {
			return attribute.get(((AbstractModelContext) context.modelContext).model);
		}
	}
}
