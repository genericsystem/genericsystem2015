package org.genericsystem.ui;

import java.util.AbstractList;
import java.util.List;
import java.util.function.Function;

import javafx.beans.binding.Bindings;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;

public interface Binder<M, SUBMODEL, WRAPPER> {

	public void init(Function<? super M, WRAPPER> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement);

	public static <N, M, SUBMODEL, X> Binder<M, SUBMODEL, X> actionBinder(Function<N, ObjectProperty<EventHandler<ActionEvent>>> applyOnNode) {
		return new Binder<M, SUBMODEL, X>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Function<? super M, X> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).set(event -> applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	// public static <N, M, SUBMODEL, X> Binder<M, SUBMODEL, ObservableValue<X>> propertySetter(Function<N, Property<X>> applyOnNode) {
	// return new Binder<M, SUBMODEL, ObservableValue<X>>() {
	// @Override
	// public void init(Function<? super M, ObservableValue<X>> applyOnModel, ModelContext<M> modelContext, ViewContext viewContext, Element<SUBMODEL> childElement) {
	// applyOnNode.apply((N) viewContext.getNode()).setValue(applyOnModel.apply(modelContext.getModel()).getValue());
	// }
	// };
	// }

	public static <N, M, SUBMODEL, W> Binder<M, SUBMODEL, ObservableValue<W>> propertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<M, SUBMODEL, ObservableValue<W>>() {
			@SuppressWarnings("unchecked")
			@Override
			public void init(Function<? super M, ObservableValue<W>> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bind(applyOnModel.apply(modelContext.getModel()));
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
		};
	}

	public static <N, M, SUBMODEL, W> Binder<M, SUBMODEL, ObservableList<W>> foreachBinder() {
		return new Binder<M, SUBMODEL, ObservableList<W>>() {
			private List<W> list;

			@Override
			public void init(Function<? super M, ObservableList<W>> applyOnModel, ModelContext<M> modelContext, ViewContext<?> viewContext, Element<SUBMODEL> childElement) {
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
				Bindings.bindContent(list, applyOnModel.apply(modelContext.getModel()));
			}
		};
	}
}
