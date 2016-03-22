package org.genericsystem.distributed.ui;

import java.util.function.Function;
import java.util.function.Supplier;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import org.genericsystem.distributed.ui.ModelContext.ModelContextList;
import com.esotericsoftware.minlog.Log;

public interface MetaBinder<N, W> {

	default void init(Function<Model, W> method, ViewContext<N> viewContext, Element<?> childElement) {
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
				assert listener == null;
				listener = change -> {
					Log.info("UUUUUUUUUUUUUUUUUUUUUUUUUU" + change);
					while (change.next()) {
						if (change.wasPermutated()) {
							for (int i = change.getFrom(); i < change.getTo(); i++)
								children.delete(change.getFrom());
							int index = change.getFrom();
							for (Model model : change.getList().subList(change.getFrom(), change.getTo()))
								children.insert(index++, model, viewContext);
						} else {
							if (change.wasRemoved())
								for (int i = 0; i < change.getRemovedSize(); i++)
									children.delete(change.getFrom());
							if (change.wasAdded()) {
								int index = change.getFrom();
								for (Model model : change.getAddedSubList())
									children.insert(index, model, viewContext);
							}
						}
					}
				};
				wrapper.addListener(new WeakListChangeListener(listener));
				int index = 0;
				for (T model : wrapper)
					children.insert(index++, model, viewContext);
			}
		};
	}
	// public static <N, T extends Model> MetaBinder<N, ObservableList<T>> selectorBinder() {
	// return new MetaBinder<N, ObservableList<T>>() {
	// private ChangeListener<Model> listener;
	//
	// @Override
	// public void init(ObservableList<T> wrapper, ViewContext<N> viewContext, Element<?> childElement) {
	// ModelContextList children = viewContext.getModelContext().getChildren(childElement);
	// wrapper.addListener(new WeakChangeListener<>(listener = children.getChangeListener(viewContext)));
	// listener.changed(wrapper, null, wrapper.getValue());
	// }
	// };
	// }
}
