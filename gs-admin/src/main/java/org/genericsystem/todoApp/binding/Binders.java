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

import org.genericsystem.todoApp.IModelContext.AbstractModelContext;
import org.genericsystem.todoApp.IModelContext.ModelContextImpl;
import org.genericsystem.todoApp.IViewContext.AbstractViewContext;

public abstract class Binders<Type> {

	public static class ClickBinder<T> {
		public static Binder<Method> methodBind() {
			return new Binder<Method>() {

				@Override
				public void init(Method val, BindingContext context) {
					Method method = val;
					((Button) (((AbstractViewContext) context.viewContext).node)).setOnAction(e -> {
						try {
							if (method.getParameterCount() == 0)
								method.invoke(((AbstractModelContext) context.modelContext).model);
							else {
								AbstractModelContext resolvedContext = ((AbstractModelContext) context.modelContext).resolve(method);
								method.invoke(resolvedContext.model, ((AbstractModelContext) context.modelContext).model);
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
			Binder<StringProperty> imp = new Binder<StringProperty>() {
				@Override
				public void init(StringProperty val, BindingContext context) {
					val.bindBidirectional(((TextField) (((AbstractViewContext) context.viewContext).node)).textProperty());
				}
			};
			return imp;
		}
	}

	public static class TextBinder {
		public static Binder<ObservableValue<String>> textBind() {
			Binder<ObservableValue<String>> imp = new Binder<ObservableValue<String>>() {
				@Override
				public void init(ObservableValue<String> val, BindingContext context) {
					if ((((AbstractViewContext) context.viewContext).node) instanceof Label)
						((Label) (((AbstractViewContext) context.viewContext).node)).textProperty().set(val.getValue());
				}
			};
			return imp;
		}
	}

	public static class ForeachBinder<T> {

		public static <T> Binder<ObservableList<T>> foreach() {

			return new Binder<ObservableList<T>>() {

				private ListChangeListener<T> changeListener;

				// private final List<IModelContext> contexts = new LinkedList<>();
				// private final List<T> values = new LinkedList<>();

				@Override
				public void init(ObservableList<T> val, BindingContext context) {
					((AbstractViewContext) context.viewContext).initContent = false;
					val.addListener(new WeakListChangeListener<T>(changeListener = change -> {
						while (change.next()) {
							if (change.wasPermutated() || change.wasUpdated())
								throw new UnsupportedOperationException();
							change.getAddedSubList().forEach(t -> {
								ModelContextImpl childContext = (ModelContextImpl) context.modelContext.createChild(t);
								context.viewContext.bind(childContext);
							});

							change.getRemoved().forEach(model -> {
								// context(model).destroy();

									// change.getList().indexOf(model);
									// System.out.println(((AbstractModelContext) context.modelContext).model.);

								});
						}
					}));
					// Consumer<ObservableList<T>> notifyImpl = notifyImpl(context);
					// notifyImpl.accept(val);
				}

				// public Consumer<ObservableList<T>> notifyImpl(BindingContext context) {
				// return myList -> {
				// myList.forEach(t -> {
				// ModelContextImpl childContext = (ModelContextImpl) context.modelContext.createChild(t);
				// context.viewContext.bind(childContext);
				// // values.add(t);
				// // contexts.add(childContext);
				// });
				// };
				// }

			};
		}
	}
}