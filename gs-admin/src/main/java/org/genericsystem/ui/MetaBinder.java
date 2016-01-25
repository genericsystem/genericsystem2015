package org.genericsystem.ui;

import java.util.function.Function;
import java.util.function.Supplier;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import org.genericsystem.ui.ModelContext.ModelContextList;

public interface MetaBinder<N, W> {

	default void init(Function<?, W> method, ViewContext<N> viewContext, Element<?> childElement) {
		init(viewContext.getModelContext().applyOnModel(method), viewContext, childElement);
	}

	default void init(Supplier<W> applyOnModel, ViewContext<N> viewContext, Element<?> childElement) {
		init(applyOnModel.get(), viewContext, childElement);
	}

	default void init(W wrapper, ViewContext<N> viewContext, Element<?> childElement) {}

	public static <N, T extends Model> MetaBinder<N, ObservableList<T>> foreachBinder() {
		return new MetaBinder<N, ObservableList<T>>() {
			private ListChangeListener<Model> listener;

			@Override
			public void init(ObservableList<T> wrapper, ViewContext<N> viewContext, Element<?> childElement) {
				ModelContextList children = viewContext.getModelContext().getChildren(childElement);
				wrapper.addListener(new WeakListChangeListener<>(listener = children.getListChangeListener(viewContext)));
				int index = 0;
				for (T model : wrapper)
					children.insert(index++, model, viewContext);
			}
		};
	}

	public static <N, T extends Model> MetaBinder<N, ObservableValue<T>> selectorBinder() {
		return new MetaBinder<N, ObservableValue<T>>() {
			private ChangeListener<Model> listener;

			@Override
			public void init(ObservableValue<T> wrapper, ViewContext<N> viewContext, Element<?> childElement) {
				ModelContextList children = viewContext.getModelContext().getChildren(childElement);
				wrapper.addListener(new WeakChangeListener<>(listener = children.getChangeListener(viewContext)));
				listener.changed(wrapper, null, wrapper.getValue());
			}
		};
	}
}
