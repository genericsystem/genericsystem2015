package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.CheckBox;

import org.genericsystem.ui.Element;

public class GSCheckBox extends GSRegion<GSCheckBox, CheckBox> {

	public GSCheckBox(Element parent) {
		super(parent, CheckBox.class);
	}

	public <M> GSCheckBox(Element parent, Function<M, Property<Boolean>> function) {
		super(parent, CheckBox.class);
		bindSelectedProperty(function);
	}

	public <M> GSCheckBox setText(Function<M, ObservableValue<String>> function) {
		addBinding(CheckBox::textProperty, function);
		return this;
	}

	public <M> GSCheckBox bindSelectedProperty(Function<M, Property<Boolean>> function) {
		addBidirectionalBinding(CheckBox::selectedProperty, function);
		return this;
	}
}
