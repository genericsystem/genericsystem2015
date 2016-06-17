package org.genericsystem.reactor.model;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.model.CompositeModel.StringExtractor;

public class InputCompositeModel extends CompositeModel {
	private Property<String> inputString = new SimpleStringProperty();

	public InputCompositeModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
	}

	public Property<String> getInputString() {
		return inputString;
	}
}