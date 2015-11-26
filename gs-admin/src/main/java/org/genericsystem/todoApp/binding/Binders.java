package org.genericsystem.todoApp.binding;

import java.lang.reflect.Method;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import org.genericsystem.todoApp.ModelContext;

public abstract class Binders<Type> {

	public static class ClickBinder<T> {
		public static Binder<Method> methodBind() {
			return new Binder<Method>() {

				@Override
				public void init(Method val, BindingContext context) {
					Method method = val;
					((Button) (context.getViewContext()).getNode()).setOnAction(e -> {
						try {
							if (method.getParameterCount() == 0)
								method.invoke(context.getModelContext().getModel());
							else {
								ModelContext resolvedContext = context.getModelContext().resolve(method);
								method.invoke(resolvedContext.getModel(), context.getModelContext().getModel());
							}
						} catch (Exception e1) {
							e1.printStackTrace();
						}
					});
				}
			};
		}
	}

	public static class EnterBinder {
		public static Binder<StringProperty> enterBind() {
			return new Binder<StringProperty>() {
				@Override
				public void init(StringProperty val, BindingContext context) {
					val.bindBidirectional(((TextField) (context.getViewContext().getNode())).textProperty());
				}
			};
		}
	}

	public static class TextBinder {
		public static Binder<ObservableValue<String>> textBind() {
			return new Binder<ObservableValue<String>>() {
				@Override
				public void init(ObservableValue<String> val, BindingContext context) {
					if ((context.getViewContext().getNode()) instanceof Label)
						((Label) (context.getViewContext().getNode())).textProperty().set(val.getValue());
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

							change.getAddedSubList().forEach(t -> {
								ModelContext childContext = context.getModelContext().createChild(t);
								context.getViewContext().bind(childContext);
							});

							change.getRemoved().forEach(model -> {
								context.getModelContext().destroyChildrenContext(model);
							});
						}
					}));
				}
			};
		}
	}
}