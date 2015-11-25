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
import org.genericsystem.todoApp.IModelContext.ModelContext;

public abstract class Binders<Type> {

	public static class ClickBinder<T> {
		public static Binder<Method> methodBind() {
			return new Binder<Method>() {

				@Override
				public void init(Method val, BindingContext context) {
					Method method = val;
					((Button) (context.viewContext).getNode()).setOnAction(e -> {
						try {
							if (method.getParameterCount() == 0)
								method.invoke(context.modelContext.getModel());
							else {
								ModelContext resolvedContext = context.modelContext.resolve(method);
								method.invoke(resolvedContext.getModel(), context.modelContext.getModel());
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
					val.bindBidirectional(((TextField) (context.viewContext.getNode())).textProperty());
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
					if ((context.viewContext.getNode()) instanceof Label)
						((Label) (context.viewContext.getNode())).textProperty().set(val.getValue());
				}
			};
			return imp;
		}
	}

	public static class ForeachBinder<T> {

		public static <T> Binder<ObservableList<T>> foreach() {

			return new Binder<ObservableList<T>>() {

				private ListChangeListener<T> changeListener;

				// List<ModelContextImpl> contexts = new ArrayList<ModelContextImpl>();

				@Override
				public void init(ObservableList<T> val, BindingContext context) {
					context.viewContext.setInitContent(false);
					val.addListener(new WeakListChangeListener<T>(changeListener = change -> {
						while (change.next()) {
							if (change.wasPermutated() || change.wasUpdated())
								throw new UnsupportedOperationException();

							change.getAddedSubList().forEach(t -> {
								ModelContext childContext = (ModelContext) context.modelContext.createChild(t);
								context.viewContext.bind(childContext);
							});

							change.getRemoved().forEach(model -> {
								context.modelContext.destroyChildrenContext(model);
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