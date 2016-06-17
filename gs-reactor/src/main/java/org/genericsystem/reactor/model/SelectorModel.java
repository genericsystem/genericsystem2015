package org.genericsystem.reactor.model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;

public class SelectorModel extends CompositeModel {
	private Property<Generic> selection = new SimpleObjectProperty<>();
	private ObservableValue<String> selectionString = Bindings.createStringBinding(() -> getStringExtractor().apply(getSelection().getValue()),
			getSelection());

	public SelectorModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
	}

	public Property<Generic> getSelection() {
		return selection;
	}

	public ObservableValue<String> getSelectionString() {
		return selectionString;
	}
}