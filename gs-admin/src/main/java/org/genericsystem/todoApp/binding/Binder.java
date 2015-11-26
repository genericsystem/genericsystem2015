package org.genericsystem.todoApp.binding;

import java.lang.reflect.Method;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import org.genericsystem.todoApp.ModelContext;

public interface Binder<T> {
	public void init(T val, BindingContext context);

	public static class ClickBinder<T> {
		public static Binder<Method> methodBind() {
			return new Binder<Method>() {
				@Override
				public void init(Method method, BindingContext context) {
					context.getViewContext().setOnAction(event -> {
						try {
							ModelContext resolvedContext = context.getModelContext().resolve(method);
							if (method.getParameterCount() == 0)
								method.invoke(resolvedContext.getModel());
							else {
								method.invoke(resolvedContext.getModel(), context.getModelContext().getModel());
							}
						} catch (Exception e) {
							throw new IllegalStateException(e);
						}
					});
				}
			};
		}
	}

	public static class TextFieldBinder {
		public static Binder<StringProperty> inputTextBind() {
			return new Binder<StringProperty>() {
				@Override
				public void init(StringProperty val, BindingContext context) {
					context.getViewContext().getTextProperty().bindBidirectional(val);
				}
			};
		}
	}

	public static class LabelBinder {
		public static Binder<ObservableValue<String>> textBind() {
			return new Binder<ObservableValue<String>>() {
				@Override
				public void init(ObservableValue<String> val, BindingContext context) {
					context.getViewContext().getTextProperty().bind(val);
				}
			};
		}
	}

	public static class ForeachBinder<T> {

		public static <T> Binder<ObservableList<T>> foreach() {

			return new Binder<ObservableList<T>>() {
				@SuppressWarnings("unused")
				private ListChangeListener<T> changeListener;

				@Override
				public void init(ObservableList<T> val, BindingContext context) {
					context.getViewContext().setInitContent(false);

					val.addListener(new WeakListChangeListener<>(changeListener = change -> {
						while (change.next()) {
							if (change.wasPermutated() || change.wasUpdated())
								throw new UnsupportedOperationException();

							for (T model : change.getAddedSubList()) {
								ModelContext childContext = context.getModelContext().createChild(model);
								context.getViewContext().bind(childContext);
							}

							for (T model : change.getRemoved()) {
								context.getModelContext().destroyChildrenContext(model);
							};
						}
					}));
				}
			};
		}
	}
}
