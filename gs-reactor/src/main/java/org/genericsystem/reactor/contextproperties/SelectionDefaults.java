package org.genericsystem.reactor.contextproperties;

import java.util.Optional;
import java.util.stream.Collectors;

import org.genericsystem.common.Generic;
import org.genericsystem.defaults.tools.BidirectionalBinding;
import org.genericsystem.reactor.Context;
import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.context.StringExtractor;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.beans.value.WeakChangeListener;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;

/**
 * @author Nicolas Feybesse
 *
 */
public interface SelectionDefaults extends ContextProperty {

	public static final String SELECTION = "selection";
	public static final String UPDATED_GENERIC = "updatedGeneric";
	public static final String SELECTION_INDEX = "selectionIndex";
	public static final String SELECTION_SHIFT = "selectionShift";
	public static final String SELECTION_STRING = "selectionString";
	public static final String CONTEXTS_LISTENER = "contextsListener";
	public static final String UPDATED_GENERIC_LISTENER = "updatedGenericListener";

	default void createSelectionProperty() {
		createNewContextProperty(SELECTION);
		addContextAttribute(SELECTION_STRING,
				model -> Bindings.createStringBinding(() -> StringExtractor.SIMPLE_CLASS_EXTRACTOR.apply(getSelectionProperty(model).getValue() != null ? getSelectionProperty(model).getValue().getGeneric() : null), getSelectionProperty(model)));
		addContextAttribute(SELECTION_INDEX, model -> {
			Property<Integer> index = new SimpleObjectProperty<>();
			index.addListener(new WeakChangeListener<>(model.getHtmlDomNode((Tag) this).getIndexListener()));
			return index;
		});
		createNewContextProperty(UPDATED_GENERIC);
	}

	default Property<Context> getSelectionProperty(Context context) {
		return getContextProperty(SELECTION, context);
	}

	default Property<Generic> getUpdatedGenericProperty(Context context) {
		return getContextProperty(UPDATED_GENERIC, context);
	}

	default Property<Integer> getSelectionIndex(Context context) {
		return getContextProperty(SELECTION_INDEX, context);
	}

	default void setSelectionShift(int shift) {
		createNewInitializedProperty(SELECTION_SHIFT, context -> shift);
	}

	default int getSelectionShift(Context context) {
		Property<Integer> selectionShiftProperty = getContextProperty(SELECTION_SHIFT, context);
		return selectionShiftProperty != null ? selectionShiftProperty.getValue() : 0;
	}

	default ObservableValue<String> getSelectionString(Context context) {
		return getContextObservableValue(SELECTION_STRING, context);
	}

	default ListChangeListener<Context> getContextsListenerProperty(Context context) {
		return getContextAttribute(CONTEXTS_LISTENER, context);
	}

	default ChangeListener<Generic> getUpdatedGenericListenerProperty(Context context) {
		return getContextAttribute(UPDATED_GENERIC_LISTENER, context);
	}

	default void bindBiDirectionalSelection(Tag subElement) {
		addPostfixBinding(modelContext -> {
			ObservableList<Context> subContexts = modelContext.getSubContexts(subElement);
			Generic selectedGeneric = modelContext.getGeneric();
			Optional<Context> selectedModel = subContexts.stream().filter(sub -> selectedGeneric.equals(sub.getGeneric())).findFirst();
			Property<Context> selection = getSelectionProperty(modelContext);
			int selectionShift = getSelectionShift(modelContext);
			selection.setValue(selectedModel.isPresent() ? selectedModel.get() : null);
			Property<Integer> selectionIndex = getSelectionIndex(modelContext);
			BidirectionalBinding.bind(selectionIndex, selection, number -> number.intValue() - selectionShift >= 0 ? (Context) subContexts.get(number.intValue() - selectionShift) : null, genericModel -> subContexts.indexOf(genericModel) + selectionShift);
			subContexts.addListener((ListChangeListener<Context>) change -> {
				if (selection != null) {
					Integer newIndex = subContexts.indexOf(selection.getValue()) + selectionShift;
					if (newIndex != selectionIndex.getValue())
						selectionIndex.setValue(newIndex);
				}
			});
		});
	}

	default void bindSelection(Tag subElement, Context context) {
		ObservableList<Context> subContexts = context.getSubContexts(subElement);
		Property<Context> selection = getSelectionProperty(context);
		Property<Generic> updatedGeneric = getUpdatedGenericProperty(context);

		setContextAttribute(CONTEXTS_LISTENER, context, (ListChangeListener<Context>) change -> {
			if (selection.getValue() != null)
				while (change.next())
					if (change.wasRemoved() && !change.wasAdded() && change.getRemoved().stream().map(c -> c.getGeneric()).collect(Collectors.toList()).contains(selection.getValue().getGeneric()))
						selection.setValue(null);
		});
		setContextAttribute(UPDATED_GENERIC_LISTENER, context, (ChangeListener<Generic>) (o, v, nv) -> {
			Optional<? extends Context> updatedContext = subContexts.stream().filter(m -> m.getGeneric().equals(nv)).findFirst();
			if (updatedContext.isPresent() && selection.getValue() == null)
				selection.setValue(updatedContext.get());
		});

		if (selection != null && updatedGeneric != null) {
			subContexts.addListener(getContextsListenerProperty(context));
			updatedGeneric.addListener(getUpdatedGenericListenerProperty(context));
		}
	}

	default void unbindSelection(Tag subElement, Context context) {
		ObservableList<Context> subContexts = context.getSubContexts(subElement);
		Property<Context> selection = getSelectionProperty(context);
		Property<Generic> updatedGeneric = getUpdatedGenericProperty(context);

		if (selection != null && updatedGeneric != null) {
			subContexts.removeListener(getContextsListenerProperty(context));
			updatedGeneric.removeListener(getUpdatedGenericListenerProperty(context));
		}
	}
}
