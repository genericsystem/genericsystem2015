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

public interface Binder<M, T> {
	public void init(Function<? super M, T> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement);

	public static <N, M, T> Binder<M, T> actionBinder(Function<N, ObjectProperty<EventHandler<ActionEvent>>> applyOnNode) {
		return new Binder<M, T>() {
			@Override
			public void init(Function<? super M, T> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).set(event -> applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <N, M, W> Binder<M, ObservableValue<W>> propertySetter(Function<N, Property<W>> applyOnNode) {
		return new Binder<M, ObservableValue<W>>() {
			@Override
			public void init(Function<? super M, ObservableValue<W>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).setValue(applyOnModel.apply(modelContext.getModel()).getValue());
			}
		};
	}

	public static <N, M, W> Binder<M, ObservableValue<W>> propertyBinder(Function<N, Property<W>> applyOnNode) {
		return new Binder<M, ObservableValue<W>>() {
			@Override
			public void init(Function<? super M, ObservableValue<W>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bind(applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <N, M> Binder<M, Property<String>> inputTextBinder(Function<N, Property<String>> applyOnNode) {
		return new Binder<M, Property<String>>() {
			@Override
			public void init(Function<? super M, Property<String>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement) {
				applyOnNode.apply((N) viewContext.getNode()).bindBidirectional(applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <N, M, W> Binder<M, ObservableList<W>> foreachBinder() {
		return new Binder<M, ObservableList<W>>() {
			private List<W> list;

			@Override
			public void init(Function<? super M, ObservableList<W>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element<M> childElement) {
				list = new AbstractList<W>() {

					@Override
					public W get(int index) {
						return modelContext.get(index).getModel();
					}

					@Override
					public int size() {
						return modelContext.size();
					}

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

					@Override
					public W remove(int index) {
						return modelContext.removeSubContext(index).getModel();
					}

				};
				Bindings.bindContent(list, applyOnModel.apply(modelContext.getModel()));
			}
		};
	}
}
