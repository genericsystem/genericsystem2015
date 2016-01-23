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

	default void init(W wrapper, ViewContext<N> viewContext, Element<?> childElement) {
	}

	public static <N, W> MetaBinder<N, ObservableList<W>> foreachBinder() {
		return new MetaBinder<N, ObservableList<W>>() {
			private ListChangeListener<W> listener;

			@Override
			public void init(ObservableList<W> wrapper, ViewContext<N> viewContext, Element<?> childElement) {
				ModelContextList children = viewContext.getModelContext().getChildren(childElement);
				wrapper.addListener(new WeakListChangeListener<>(listener = children.getListChangeListener(viewContext)));
				int index = 0;
				for (W model : wrapper)
					children.insert(index++, model, viewContext);
			}
		};
	}

	public static <N, W> MetaBinder<N, ObservableValue<W>> selectorBinder() {
		return new MetaBinder<N, ObservableValue<W>>() {
			private ChangeListener<W> listener;

			@Override
			public void init(ObservableValue<W> wrapper, ViewContext<N> viewContext, Element<?> childElement) {
				ModelContextList children = viewContext.getModelContext().getChildren(childElement);
				wrapper.addListener(new WeakChangeListener<>(listener = viewContext.getModelContext().getChildren(childElement).getChangeListener(viewContext)));
				listener.changed(wrapper, null, wrapper.getValue());
			}
		};
	}
}
