package org.genericsystem.reactor.gs;

import java.util.Optional;
import java.util.function.Consumer;

import javafx.beans.property.Property;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.ReactorStatics;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public interface SelectionDefaults {

	// void addPrefixBinding(Consumer<GenericModel> consumer);

	void addPostfixBinding(Consumer<GenericModel> consumer);

	void createNewProperty(String propertyName);

	<T> Property<T> getProperty(String property, Model model);

	default void createSelectionProperty() {
		createNewProperty(ReactorStatics.SELECTION);
	}

	default Property<GenericModel> getSelectionProperty(GenericModel model) {
		return getProperty(ReactorStatics.SELECTION, model);
	}

	default void bindBiDirectionalSelection(Tag<GenericModel> subElement) {
		addPostfixBinding(modelContext -> {
			ObservableList<GenericModel> subContexts = modelContext.getSubContexts(subElement);
			Generic selectedGeneric = modelContext.getGeneric();
			Optional<GenericModel> selectedModel = subContexts.stream().filter(sub -> selectedGeneric.equals(sub.getGeneric())).findFirst();
			Property<GenericModel> selection = getSelectionProperty(modelContext);// getProperty(ReactorStatics.SELECTION, modelContext);
			int selectionShift = getProperty(ReactorStatics.SELECTION_SHIFT, modelContext) != null ? (Integer) getProperty(ReactorStatics.SELECTION_SHIFT, modelContext).getValue() : 0;
			selection.setValue(selectedModel.isPresent() ? selectedModel.get() : null);
			Property<Number> selectionIndex = getProperty(ReactorStatics.SELECTION_INDEX, modelContext);
			BidirectionalBinding.bind(selectionIndex, selection, number -> number.intValue() - selectionShift >= 0 ? (GenericModel) subContexts.get(number.intValue() - selectionShift) : null, genericModel -> subContexts.indexOf(genericModel)
					+ selectionShift);
			subContexts.addListener((ListChangeListener<GenericModel>) change -> {
				if (selection != null) {
					Number oldIndex = (Number) getProperty(ReactorStatics.SELECTION_INDEX, modelContext).getValue();
					Number newIndex = subContexts.indexOf(selection.getValue()) + selectionShift;
					if (newIndex != oldIndex)
						this.getProperty(ReactorStatics.SELECTION_INDEX, modelContext).setValue(newIndex);
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
						if (change.wasRemoved() && !change.wasAdded())
							if (change.getRemoved().contains(selection.getValue()))
								selection.setValue(null);
			});
		});
	}
}
