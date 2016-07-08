package org.genericsystem.reactor.model;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

import javafx.beans.binding.Bindings;
import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

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

	@Override
	public SelectorModel duplicate(Model parent) {
		SelectorModel model = new SelectorModel(getGenerics(), getStringExtractor());
		model.parent = parent;
		model.selection = this.selection;
		model.selectionString = this.selectionString;
		return model;
	}
}