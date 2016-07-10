package org.genericsystem.reactor.model;

import java.io.Serializable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;

import org.genericsystem.common.Generic;

public class InputCheckModel extends GenericModel implements InputableModel {

	private Property<Boolean> checked = new SimpleBooleanProperty(getGeneric().getValue() instanceof Boolean ? (Boolean) getGeneric().getValue() : false);
	private Property<TriFunction<Generic[], Serializable, Generic, Generic>> inputAction = new SimpleObjectProperty<>();

	public InputCheckModel(Generic[] generics, StringExtractor stringExtractor) {
		super(generics, stringExtractor);
	}

	public Property<Boolean> getChecked() {
		return checked;
	}

	@Override
	public Serializable getValue() {
		return checked.getValue();
	}

	@Override
	public Property<TriFunction<Generic[], Serializable, Generic, Generic>> getInputAction() {
		return inputAction;
	}
}
