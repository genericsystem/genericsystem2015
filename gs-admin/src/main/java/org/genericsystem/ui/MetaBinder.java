package org.genericsystem.ui;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

public interface MetaBinder<N, W> {

	default void init(Function<?, W> method, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
		init(modelContext.applyOnModel(method), modelContext, viewContext, childElement);
	}

	default void init(Supplier<W> applyOnModel, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
		init(applyOnModel.get(), modelContext, viewContext, childElement);
	}

	default void init(W wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
	}

	public static <N, W> MetaBinder<N, ObservableList<W>> foreachBinder() {
		return new MetaBinder<N, ObservableList<W>>() {

			@Override
			public void init(ObservableList<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {

				List<ModelContext> children = modelContext.getChildren(childElement);

				class ForEachList extends AbstractList<W> implements ListChangeListener<W> {
					{
						addAll(wrapper);
					}

					@SuppressWarnings("unchecked")
					@Override
					public W get(int index) {
						return (W) children.get(index).getModel();
					}

					@Override
					public int size() {
						return children.size();
					}

					@Override
					public void add(int index, W model) {
						ModelContext childContext = new ModelContext(modelContext, model);
						new ViewContext<>(viewContext, childContext, childElement, null);
						children.add(index, childContext);
					}

					@Override
					public W set(int index, W element) {
						W remove = remove(index);
						add(index, element);
						return remove;
					}

					@Override
					public W remove(int index) {
						return children.remove(index).destroy();
					}

					@Override
					public void onChanged(Change<? extends W> change) {
						while (change.next()) {
							if (change.wasPermutated()) {
								subList(change.getFrom(), change.getTo()).clear();
								addAll(change.getFrom(), change.getList().subList(change.getFrom(), change.getTo()));
							} else {
								if (change.wasRemoved())
									subList(change.getFrom(), change.getFrom() + change.getRemovedSize()).clear();
								if (change.wasAdded())
									addAll(change.getFrom(), change.getAddedSubList());
							}
						}
					}
				}
				wrapper.addListener(new ForEachList());
			}
		};
	}

	public static <N, W> MetaBinder<N, ObservableValue<W>> selectorBinder() {
		return new MetaBinder<N, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> wrapper, ModelContext modelContext, ViewContext<N> viewContext, Element<?> childElement) {
				List<ModelContext> children = modelContext.getChildren(childElement);
				Consumer<W> consumer = (newModel) -> {
					if (newModel != null) {
						ModelContext childContext = new ModelContext(modelContext, newModel);
						new ViewContext<>(viewContext, childContext, childElement, null);
						children.add(childContext);
						assert children.size() == 1;
					}
				};
				wrapper.addListener((o, oldModel, newModel) -> {
					if (oldModel == newModel)
						return;
					if (oldModel != null)
						children.remove(0).destroy();
					consumer.accept(newModel);
				});
				consumer.accept(wrapper.getValue());
			}
		};
	}
}
