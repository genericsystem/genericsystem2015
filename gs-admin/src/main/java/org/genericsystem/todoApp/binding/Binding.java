package org.genericsystem.todoApp.binding;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

import javafx.beans.Observable;

import org.genericsystem.todoApp.IModelContext.ModelContext;

public interface Binding {

	public abstract void init(BindingContext context);

	public static <U extends Observable, T> FieldBinding<U, T> bindToField(Class<?> clazz, String fieldName, Binder<U> binder) {
		try {
			return new FieldBinding<>(clazz.getField(fieldName), binder);
		} catch (NoSuchFieldException | SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

	public static MethodBinding bindToMethod(Class<?> methodClass, String methodName, Binder<Method> binder) {
		try {
			return new MethodBinding(methodClass.getMethod(methodName), binder);
		} catch (NoSuchMethodException | SecurityException e1) {
			throw new IllegalStateException(e1);
		}
	}

	public static MethodBinding bindToMethod(Class<?> methodClass, String methodName, Binder<Method> binder, Class<?> methodParameterClass) {
		try {
			return new MethodBinding(methodClass.getMethod(methodName, methodParameterClass), binder);
		} catch (NoSuchMethodException | SecurityException e1) {
			throw new IllegalStateException(e1);
		}
	}

	public static abstract class AbstractBinding<T> implements Binding {
		private Binder<T> binder;

		public AbstractBinding(Binder<T> binder) {
			this.binder = binder;
		}

		@Override
		public void init(BindingContext context) {
			T model = resolve(context);
			binder.init(model, context);

		}

		protected abstract T resolve(BindingContext context);

	}

	static class FieldBinding<U extends Observable, T> extends AbstractBinding<U> {
		private Field attribute;

		public FieldBinding(Field attribute, Binder<U> binder) {
			super(binder);
			this.attribute = attribute;
		}

		@Override
		protected U resolve(BindingContext context) {
			ModelContext resolvedContext = context.getModelContext().resolve(attribute);
			try {
				return (U) attribute.get(resolvedContext.getModel());
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
	}

	static class MethodBinding extends AbstractBinding<Method> {
		private Method method;

		public MethodBinding(Method method, Binder<Method> binder) {
			super(binder);
			this.method = method;
		}

		@Override
		protected Method resolve(BindingContext context) {
			// AbstractModelContext resolvedContext = ((AbstractModelContext) context.modelContext).resolve(method);
			return method;
		}
	}

}
