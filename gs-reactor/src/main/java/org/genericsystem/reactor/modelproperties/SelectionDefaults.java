package org.genericsystem.reactor.modelproperties;

import java.util.Optional;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;
import org.genericsystem.reactor.model.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public interface SelectionDefaults extends ModelProperty<GenericModel> {

	public static final String SELECTION = "selection";
	public static final String UPDATED_GENERIC = "updatedGeneric";
	public static final String SELECTION_INDEX = "selectionIndex";
	public static final String SELECTION_SHIFT = "selectionShift";
	public static final String SELECTION_STRING = "selectionString";

	default void createSelectionProperty() {
		createNewProperty(SELECTION);
		storeProperty(SELECTION_STRING,
				model -> Bindings.createStringBinding(() -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(getSelectionProperty(model).getValue() != null ? getSelectionProperty(model).getValue().getGeneric() : null), getSelectionProperty(model)));
		storeProperty(SELECTION_INDEX, model -> {
			Property<Integer> index = new SimpleObjectProperty<>();
			index.addListener(new WeakChangeListener<>(model.getViewContext((Tag<?>) this).getNode().getIndexListener()));
			return index;
		});
		createNewProperty(UPDATED_GENERIC);
	}

	default Property<GenericModel> getSelectionProperty(GenericModel model) {
		return getProperty(SELECTION, model);
	}

	default Property<Generic> getUpdatedGenericProperty(GenericModel model) {
		return getProperty(UPDATED_GENERIC, model);
	}

	default Property<Integer> getSelectionIndex(GenericModel model) {
		return getProperty(SELECTION_INDEX, model);
	}

	default void setSelectionShift(int shift) {
		createNewInitializedProperty(SELECTION_SHIFT, model -> shift);
	}

	default int getSelectionShift(GenericModel model) {
		Property<Integer> selectionShiftProperty = getProperty(SELECTION_SHIFT, model);
		return selectionShiftProperty != null ? selectionShiftProperty.getValue() : 0;
	}

	default ObservableValue<String> getSelectionString(GenericModel model) {
		return getObservableValue(SELECTION_STRING, model);
	}

	default void bindBiDirectionalSelection(Tag<GenericModel> subElement) {
		addPostfixBinding(modelContext -> {
			ObservableList<GenericModel> subContexts = modelContext.getSubContexts(subElement);
			Generic selectedGeneric = modelContext.getGeneric();
			Optional<GenericModel> selectedModel = subContexts.stream().filter(sub -> selectedGeneric.equals(sub.getGeneric())).findFirst();
			Property<GenericModel> selection = getSelectionProperty(modelContext);
			int selectionShift = getSelectionShift(modelContext);
			selection.setValue(selectedModel.isPresent() ? selectedModel.get() : null);
			Property<Integer> selectionIndex = getSelectionIndex(modelContext);
			BidirectionalBinding.bind(selectionIndex, selection, number -> number.intValue() - selectionShift >= 0 ? (GenericModel) subContexts.get(number.intValue() - selectionShift) : null,
					genericModel -> subContexts.indexOf(genericModel) + selectionShift);
			subContexts.addListener((ListChangeListener<GenericModel>) change -> {
				if (selection != null) {
					Integer newIndex = subContexts.indexOf(selection.getValue()) + selectionShift;
					if (newIndex != selectionIndex.getValue())
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
