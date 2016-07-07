package org.genericsystem.reactor.model;

import java.io.Serializable;

import org.genericsystem.common.Generic;
import org.genericsystem.reactor.Model;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleBooleanProperty;
import javafx.beans.property.SimpleObjectProperty;

public class InputCheckModel extends GenericModel implements InputableModel {
	private Property<Boolean> checked = new SimpleBooleanProperty(
			getGenerics()[0].getValue() instanceof Boolean ? (Boolean) getGenerics()[0].getValue() : false);
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

	@Override
	public InputCheckModel duplicate(Model parent) {
		InputCheckModel model = new InputCheckModel(getGenerics(), getStringExtractor());
		model.parent = parent;
		model.checked = this.checked;
		model.inputAction = this.inputAction;
		return model;
	}
}
