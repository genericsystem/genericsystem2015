package org.genericsystem.todoApp.binding;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import javafx.beans.Observable;
import org.genericsystem.todoApp.IModelContext.ModelContext;

public interface Binding {

	public abstract void init(BindingContext context);

	public static <U extends Observable, T> FieldBinding<U, T> bindTo(Field attribute, Binder<U> binder) {
		return new FieldBinding<>(attribute, binder);
	}

	public static MethodBinding bindTo(Method method, Binder<Method> binder) {
		return new MethodBinding(method, binder);
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
			ModelContext resolvedContext = context.modelContext.resolve(attribute);
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
