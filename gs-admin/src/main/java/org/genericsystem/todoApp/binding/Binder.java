package org.genericsystem.todoApp.binding;

import java.lang.reflect.Method;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import org.genericsystem.todoApp.ModelContext;

public interface Binder<T> {
	public void init(T val, BindingContext context);

	public static Binder<Method> methodBind() {
		return new Binder<Method>() {
			@Override
			public void init(Method method, BindingContext context) {
				context.getViewContext().setOnAction(event -> {
					try {
						Object resolvedContextModel = context.getModelContext().resolve(method.getDeclaringClass()).getModel();
						if (method.getParameterCount() == 0)
							method.invoke(resolvedContextModel);
						else
							method.invoke(resolvedContextModel, context.getModelContext().getModel());
					} catch (Exception e) {
						throw new IllegalStateException(e);
					}
				});
			}
		};
	}

	public static Binder<Consumer<Object>> taskBind() {
		return new Binder<Consumer<Object>>() {
			@Override
			public void init(Consumer<Object> consumer, BindingContext context) {
				context.getViewContext().setOnAction(event -> consumer.accept(context.getModelContext().getModel()));
			}
		};
	}

	public static Binder<StringProperty> inputTextBind() {
		return new Binder<StringProperty>() {
			@Override
			public void init(StringProperty val, BindingContext context) {
				context.getViewContext().getTextProperty().bindBidirectional(val);
			}
		};
	}

	public static Binder<ObservableValue<String>> textBind() {
		return new Binder<ObservableValue<String>>() {
			@Override
			public void init(ObservableValue<String> val, BindingContext context) {
				context.getViewContext().getTextProperty().bind(val);
			}
		};
	}

	public static <T> Binder<ObservableList<T>> foreach() {

		return new Binder<ObservableList<T>>() {
			@SuppressWarnings("unused")
			private ListChangeListener<T> changeListener;

			@Override
			public void init(ObservableList<T> val, BindingContext context) {
				context.getViewContext().disableInitChildren();
				Function<T, ModelContext> createChildContext = t -> context.getModelContext().createChild(t, context.getViewContext());
				List<ModelContext> children = context.getModelContext().getChildren();
				children.addAll(val.stream().map(createChildContext).collect(Collectors.toList()));
				val.addListener(new WeakListChangeListener<>(changeListener = change -> {
					while (change.next()) {
						if (change.wasPermutated()) {
							children.subList(change.getFrom(), change.getTo()).clear();
							children.addAll(change.getFrom(), change.getList().subList(change.getFrom(), change.getTo()).stream().map(createChildContext).collect(Collectors.toList()));
						} else {
							if (change.wasRemoved())
								children.subList(change.getFrom(), change.getFrom() + change.getRemovedSize()).clear();
							if (change.wasAdded())
								children.addAll(change.getFrom(), change.getAddedSubList().stream().map(createChildContext).collect(Collectors.toList()));
						}

					}
				}));
			}
		};
	}
}
