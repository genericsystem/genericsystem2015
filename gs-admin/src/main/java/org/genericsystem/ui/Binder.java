package org.genericsystem.ui;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Function;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;

public interface Binder<M, SUBMODEL, WRAPPER> {

	default void init(Function<? super M, WRAPPER> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
		WRAPPER wrapper = applyOnModel.apply(modelContext.getModel());
		System.out.println("ZZZZZZZZZZ" + wrapper);
		init(wrapper, modelContext, viewContext, childElement);
	}

	public void init(WRAPPER wrapper, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement);

	public static <N, M, SUBMODEL, X extends Event> Binder<M, SUBMODEL, ObjectProperty<EventHandler<X>>> actionBinder(Function<N, ObjectProperty<EventHandler<X>>> applyOnNode) {
		return new Binder<M, SUBMODEL, ObjectProperty<EventHandler<X>>>() {
			@Override
			public void init(ObjectProperty<EventHandler<X>> wrapper, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).set(event -> {
					wrapper.get().handle(event);
				});
			}
		};
	}

	public static <N, M, SUBMODEL, W> Binder<M, SUBMODEL, ObservableValue<W>> propertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<M, SUBMODEL, ObservableValue<W>>() {
			@Override
			public void init(ObservableValue<W> wrapper, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bind(wrapper);
			}
		};
	}

	public static <N, M, SUBMODEL> Binder<M, SUBMODEL, Property<String>> inputTextBinder(Function<N, Property<String>> applyOnNode) {
		return new Binder<M, SUBMODEL, Property<String>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Function<? super M, Property<String>> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bindBidirectional(applyOnModel.apply(modelContext.getModel()));
			}

			@Override
			public void init(Property<String> wrapper, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bindBidirectional(wrapper);
			}
		};
	}

	public static <N, M, SUBMODEL, W> Binder<M, SUBMODEL, ObservableList<W>> foreachBinder() {
		return new Binder<M, SUBMODEL, ObservableList<W>>() {

			private List<W> list;

			@Override
			public void init(ObservableList<W> wrapper, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {

				list = new AbstractList<W>() {

					@SuppressWarnings("unchecked")
					@Override
					public W get(int index) {
						return (W) modelContext.get(index).getModel();
					}

					@Override
					public int size() {
						return modelContext.size();
					}

					@SuppressWarnings("unchecked")
					@Override
					public void add(int index, W element) {
						modelContext.createSubContext(viewContext, index, element, (Element<W>) childElement);
					}

					@Override
					public W set(int index, W element) {
						W remove = remove(index);
						add(index, element);
						return remove;
					}

					@SuppressWarnings("unchecked")
					@Override
					public W remove(int index) {
						return (W) modelContext.removeSubContext(index).getModel();
					}

				};
				wrapper.addListener(new ListContentBinding<>(list));
				// Bindings.bindContent(list, wrapper);
			}
		};
	}

	public static class ListContentBinding<E> implements ListChangeListener<E> {

		private final List<E> list;

		public ListContentBinding(List<E> list) {
			this.list = list;
		}

		@Override
		public void onChanged(Change<? extends E> change) {
			while (change.next()) {
				if (change.wasPermutated()) {
					list.subList(change.getFrom(), change.getTo()).clear();
					list.addAll(change.getFrom(), change.getList().subList(change.getFrom(), change.getTo()));
				} else {
					if (change.wasRemoved()) {
						list.subList(change.getFrom(), change.getFrom() + change.getRemovedSize()).clear();
					}
					if (change.wasAdded()) {
						list.addAll(change.getFrom(), change.getAddedSubList());
					}
				}
			}
		}
	}
}
