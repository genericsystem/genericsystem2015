package org.genericsystem.reactor.modelproperties;

import java.util.Optional;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;

import javafx.beans.property.Property;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public interface SelectionDefaults extends ModelProperty<GenericModel> {

	public static final String SELECTION = "selection";
	public static final String UPDATED_GENERIC = "updatedGeneric";

	default void createSelectionProperty() {
		createNewProperty(SELECTION);
		createNewProperty(UPDATED_GENERIC);
	}

	default Property<GenericModel> getSelectionProperty(GenericModel model) {
		return getProperty(SELECTION, model);
	}

	default Property<Generic> getUpdatedGenericProperty(GenericModel model) {
		return getProperty(UPDATED_GENERIC, model);
	}

	default void bindBiDirectionalSelection(Tag<GenericModel> subElement) {
		addPostfixBinding(modelContext -> {
			ObservableList<GenericModel> subContexts = modelContext.getSubContexts(subElement);
			Generic selectedGeneric = modelContext.getGeneric();
			Optional<GenericModel> selectedModel = subContexts.stream().filter(sub -> selectedGeneric.equals(sub.getGeneric())).findFirst();
			Property<GenericModel> selection = getSelectionProperty(modelContext);// getProperty(ReactorStatics.SELECTION, modelContext);
			Property<Integer> selectionShiftProperty = getProperty(ReactorStatics.SELECTION_SHIFT, modelContext);
			int selectionShift = selectionShiftProperty != null ? selectionShiftProperty.getValue() : 0;
			selection.setValue(selectedModel.isPresent() ? selectedModel.get() : null);
			Property<Number> selectionIndex = getProperty(ReactorStatics.SELECTION_INDEX, modelContext);
			BidirectionalBinding.bind(selectionIndex, selection, number -> number.intValue() - selectionShift >= 0 ? (GenericModel) subContexts.get(number.intValue() - selectionShift) : null,
					genericModel -> subContexts.indexOf(genericModel) + selectionShift);
			subContexts.addListener((ListChangeListener<GenericModel>) change -> {
				if (selection != null) {
					Property<Number> oldIndex = getProperty(ReactorStatics.SELECTION_INDEX, modelContext);
					Number newIndex = subContexts.indexOf(selection.getValue()) + selectionShift;
					if (newIndex != oldIndex.getValue())
						selectionIndex.setValue(newIndex);
				}
			});
		});
	}

	default void bindSelection(Tag<GenericModel> subElement) {
		addPostfixBinding(model -> {
			ObservableList<GenericModel> subContexts = model.getSubContexts(subElement);
			Property<GenericModel> selection = getSelectionProperty(model);
			subContexts.addListener((ListChangeListener<GenericModel>) change -> {
				if (selection != null)
					while (change.next())
						if (change.wasRemoved() && !change.wasAdded() && change.getRemoved().contains(selection.getValue()))
							selection.setValue(null);
			});
			Property<Generic> updatedGeneric = getProperty(UPDATED_GENERIC, model);
			if (selection != null && updatedGeneric != null)
				updatedGeneric.addListener((o, v, nv) -> {
					Optional<? extends GenericModel> updatedModel = subContexts.stream().filter(m -> m.getGeneric().equals(nv)).findFirst();
					if (updatedModel.isPresent())
						getSelectionProperty(model).setValue(updatedModel.get());
				});
		});
	}
}
