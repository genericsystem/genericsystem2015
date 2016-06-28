package org.genericsystem.reactor.model;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;

import org.genericsystem.common.Generic;

public class InputCompositeModel extends CompositeModel {
	private Property<String> inputString = new SimpleStringProperty();
	private Property<TriFunction<Generic[], String, Generic, Generic>> inputAction = new SimpleObjectProperty<>();

	public InputCompositeModel(Generic[] generics, StringExtractor extractor) {
		super(generics, extractor);
	}

	public Property<String> getInputString() {
		return inputString;
	}

	public Property<TriFunction<Generic[], String, Generic, Generic>> getInputAction() {
		return inputAction;
	}

	@FunctionalInterface
	public interface TriFunction<T, U, R, S> {

	    R apply(T t, U u, S s);
	}
}