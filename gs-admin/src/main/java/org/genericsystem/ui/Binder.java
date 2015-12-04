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

public interface Binder<MODEL, T> {
	public void init(Function<MODEL, T> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement);

	public static <NODE, MODEL, T> Binder<MODEL, T> actionBinder(Function<NODE, ObjectProperty<EventHandler<ActionEvent>>> applyOnNode) {
		return new Binder<MODEL, T>() {
			@Override
			public void init(Function<MODEL, T> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				applyOnNode.apply((NODE) viewContext.getNode()).set(event -> applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <NODE, MODEL, W> Binder<MODEL, W> setter(Function<NODE, Property<W>> applyOnNode, W value) {
		return new Binder<MODEL, W>() {
			@Override
			public void init(Function<MODEL, W> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				applyOnNode.apply((NODE) viewContext.getNode()).setValue(value);
			}
		};
	}

	public static <NODE, MODEL, W> Binder<MODEL, ObservableValue<W>> propertySetter(Function<NODE, Property<W>> applyOnNode) {
		return new Binder<MODEL, ObservableValue<W>>() {
			@Override
			public void init(Function<MODEL, ObservableValue<W>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				applyOnNode.apply((NODE) viewContext.getNode()).setValue(applyOnModel.apply(modelContext.getModel()).getValue());
			}
		};
	}

	public static <NODE, MODEL, W> Binder<MODEL, ObservableValue<W>> propertyBinder(Function<NODE, Property<W>> applyOnNode) {
		return new Binder<MODEL, ObservableValue<W>>() {
			@Override
			public void init(Function<MODEL, ObservableValue<W>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				applyOnNode.apply((NODE) viewContext.getNode()).bind(applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <NODE, MODEL> Binder<MODEL, Property<String>> inputTextBinder(Function<NODE, Property<String>> applyOnNode) {
		return new Binder<MODEL, Property<String>>() {
			@Override
			public void init(Function<MODEL, Property<String>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {
				applyOnNode.apply((NODE) viewContext.getNode()).bindBidirectional(applyOnModel.apply(modelContext.getModel()));
			}
		};
	}

	public static <MODEL, SUBMODEL> Binder<MODEL, ObservableList<SUBMODEL>> foreachBinder() {

		return new Binder<MODEL, ObservableList<SUBMODEL>>() {

			private List<SUBMODEL> list;

			@Override
			public void init(Function<MODEL, ObservableList<SUBMODEL>> applyOnModel, ModelContext modelContext, ViewContext viewContext, Element childElement) {

				list = new AbstractList<SUBMODEL>() {

					@Override
					public SUBMODEL get(int index) {
						return modelContext.get(index).getModel();
					}

					@Override
					public int size() {
						return modelContext.size();
					}

					@Override
					public void add(int index, SUBMODEL element) {
						modelContext.createSubContext(viewContext, index, element, childElement);
					}

					@Override
					public SUBMODEL set(int index, SUBMODEL element) {
						SUBMODEL remove = remove(index);
						add(index, element);
						return remove;
					}

					@Override
					public SUBMODEL remove(int index) {
						return modelContext.removeSubContext(index).getModel();
					}

				};
				Bindings.bindContent(list, applyOnModel.apply(modelContext.getModel()));
			}
		};
	}
}
