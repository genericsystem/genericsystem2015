package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;

import org.genericsystem.reactor.ModelContext.ModelContextList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <W>
 */
public interface MetaBinder<W extends ObservableList<?>> {

	default void init(Function<Model, W> applyOnModel, ViewContext<?> viewContext, Element<?> childElement) {
		init(applyOnModel.apply(viewContext.getModelContext().getModel()), viewContext, childElement);
	}

	default void init(Supplier<W> applyOnModel, ViewContext<?> viewContext, Element<?> childElement) {
		init(applyOnModel.get(), viewContext, childElement);
	}

	default void init(W wrapper, ViewContext<?> viewContext, Element<?> childElement) {
	}

	public static <T extends Model> MetaBinder<ObservableList<T>> foreachBinder() {
		return new MetaBinder<ObservableList<T>>() {

			private List<ListChangeListener<Model>> listeners = new ArrayList<>();

			@Override
			public void init(ObservableList<T> wrapper, ViewContext<?> viewContext, Element<?> childElement) {
				ModelContextList children = viewContext.getModelContext().getChildren(childElement);

				ListChangeListener<Model> listener = (ListChangeListener<Model>) change -> {
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
									children.insert(index++, model, viewContext);
							}
						}
					}
				};
				listeners.add(listener);
				wrapper.addListener(new WeakListChangeListener<>(listener));
				int index = 0;
				for (T model : wrapper)
					children.insert(index++, model, viewContext);
			}
		};
	}
}
