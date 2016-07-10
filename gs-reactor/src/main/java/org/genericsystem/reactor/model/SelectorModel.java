package org.genericsystem.reactor.model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;

public class SelectorModel extends GenericModel {
	protected Property<GenericModel> selection = new SimpleObjectProperty<GenericModel>();
	protected ObservableValue<String> selectionString = Bindings.createStringBinding(
			() -> getStringExtractor().apply(getSelection().getValue() != null ? getSelection().getValue().getGeneric() : null), getSelection());

	public SelectorModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
	}

	public Property<GenericModel> getSelection() {
		return selection;
	}

	public ObservableValue<String> getSelectionString() {
		return selectionString;
	}
}