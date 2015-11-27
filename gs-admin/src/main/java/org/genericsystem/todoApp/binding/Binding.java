package org.genericsystem.todoApp.binding;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.function.Consumer;
import java.util.function.Function;
import javafx.beans.Observable;
import org.genericsystem.todoApp.ModelContext;

public abstract class Binding<T> {

	public static <U extends Observable, V> MethodBinding2<U, V> bindToMethod(Class<?> clazz, Function<V, U> function, Binder<U> binder) {
		try {
			return new MethodBinding2<>(clazz, function, binder);
		} catch (SecurityException e) {
			throw new IllegalStateException(e);
		}
	}

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

	public static TaskBinding bindToTask(Class<?> clazz, String methodName, Binder<Consumer<Object>> binder) {
		try {
			return new TaskBinding(clazz.getMethod(methodName), binder);
		} catch (SecurityException | NoSuchMethodException e) {
			throw new IllegalStateException(e);
		}
	}

	public static TaskBinding bindToTask(Class<?> clazz, String methodName, Binder<Consumer<Object>> binder, Class<?> methodParameterClass) {
		try {
			return new TaskBinding(clazz.getMethod(methodName, methodParameterClass), binder);
		} catch (SecurityException | NoSuchMethodException e) {
			throw new IllegalStateException(e);
		}
	}

	public static MethodBinding bindToMethod(Class<?> methodClass, String methodName, Binder<Method> binder, Class<?> methodParameterClass) {
		try {
			return new MethodBinding(methodClass.getMethod(methodName, methodParameterClass), binder);
		} catch (NoSuchMethodException | SecurityException e1) {
			throw new IllegalStateException(e1);
		}
	}

	private Binder<T> binder;

	public Binding(Binder<T> binder) {
		this.binder = binder;
	}

	public void init(BindingContext context) {
		T param = buildParam(context);
		binder.init(param, context);
	}

	protected abstract T buildParam(BindingContext context);

	static class MethodBinding extends Binding<Method> {
		private Method method;

		public MethodBinding(Method method, Binder<Method> binder) {
			super(binder);
			this.method = method;
		}

		@Override
		protected Method buildParam(BindingContext context) {
			// AbstractModelContext resolvedContext = ((AbstractModelContext) context.modelContext).resolve(method);
			return method;
		}
	}

	public interface Task {
		void doWork();
	}

	static class TaskBinding extends Binding<Consumer<Object>> {
		private Method method;

		public TaskBinding(Method method, Binder<Consumer<Object>> binder) {
			super(binder);
			this.method = method;
		}

		@Override
		protected Consumer<Object> buildParam(BindingContext context) {
			ModelContext resolvedContext = context.getModelContext().resolve(method.getDeclaringClass());
			return (o) -> {
				try {
					if (method.getParameterCount() == 0)
						method.invoke(resolvedContext.getModel());
					else
						method.invoke(resolvedContext.getModel(), o);
				} catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException e) {
					throw new IllegalStateException(e);
				}
			};

		}
	}

	static class MethodBinding2<U extends Observable, V> extends Binding<U> {
		private final Function<V, U> getter;
		private final Class<?> uClass;

		public MethodBinding2(Class<?> uClass, Function<V, U> getter, Binder<U> binder) {
			super(binder);
			this.getter = getter;
			this.uClass = uClass;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected U buildParam(BindingContext context) {
			ModelContext resolvedContext = context.getModelContext().resolve(uClass);
			return getter.apply((V) resolvedContext.getModel());
		}
	}

	static class FieldBinding<U extends Observable, T> extends Binding<U> {
		private Field field;

		public FieldBinding(Field field, Binder<U> binder) {
			super(binder);
			this.field = field;
		}

		@SuppressWarnings("unchecked")
		@Override
		protected U buildParam(BindingContext context) {
			ModelContext resolvedContext = context.getModelContext().resolve(field.getDeclaringClass());
			try {
				return (U) field.get(resolvedContext.getModel());
			} catch (IllegalArgumentException | IllegalAccessException e) {
				throw new IllegalStateException(e);
			}
		}
	}
}
