package org.genericsystem.reactor;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;
import java.util.function.Supplier;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.collections.WeakListChangeListener;
import org.genericsystem.reactor.CompositeModel.ModelConstructor;
import org.genericsystem.reactor.CompositeModel.ObservableListExtractor;
import org.genericsystem.reactor.CompositeModel.StringExtractor;
import org.genericsystem.reactor.ModelContext.ModelContextList;

/**
 * @author Nicolas Feybesse
 *
 * @param <N>
 * @param <W>
 */
public interface MetaBinder<N, W extends ObservableList<?>> {

	default void init(Function<Model, W> method, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
		init(viewContext.getModelContext().applyOnModel(method), viewContext, childElement);
	}

	default void init(Supplier<W> applyOnModel, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
		init(applyOnModel.get(), viewContext, childElement);
	}

	default void init(W wrapper, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
	}

	public static <N, T extends Model> MetaBinder<N, ObservableList<T>> foreachBinder() {
		return new MetaBinder<N, ObservableList<T>>() {

			private List<ListChangeListener<Model>> listeners = new ArrayList<>();

			@Override
			public void init(ObservableList<T> wrapper, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
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

	public static <N> MetaBinder<N, ObservableList<CompositeModel>> foreachBinder(StringExtractor stringExtractor, ObservableListExtractor observableListExtractor, ModelConstructor<CompositeModel> constructor) {
		return new MetaBinder<N, ObservableList<CompositeModel>>() {

			private List<ListChangeListener<Model>> listeners = new ArrayList<>();

			@Override
			public void init(Function<Model, ObservableList<CompositeModel>> method, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
				assert stringExtractor != null;
				assert observableListExtractor != null;
				assert constructor != null;
				((CompositeModel) viewContext.getModelContext().getModel()).initSubModels(observableListExtractor, gs -> constructor.build(gs, stringExtractor));
				MetaBinder.super.init(method, viewContext, childElement);
			}

			@Override
			public void init(ObservableList<CompositeModel> wrapper, ViewContext<?, N> viewContext, Element<?, ?> childElement) {
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
				for (CompositeModel model : wrapper)
					children.insert(index++, model, viewContext);
			}
		};
	}
}
