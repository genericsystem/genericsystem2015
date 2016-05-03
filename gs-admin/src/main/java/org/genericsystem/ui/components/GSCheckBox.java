package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.beans.value.ObservableValue;
import javafx.scene.control.CheckBox;

import org.genericsystem.distributed.ui.Element;

public class GSCheckBox extends GSRegion<GSCheckBox, CheckBox> {

	public GSCheckBox(Element<?> parent) {
		super(parent, CheckBox.class);
	}

	public <M> GSCheckBox(Element<?> parent, Function<M, Property<Boolean>> selectedProperty) {
		super(parent, CheckBox.class);
		bindBidirectional(selectedProperty);
	}

	public <M> GSCheckBox setObservableTextProperty(Function<M, ObservableValue<String>> observableText) {
		addBinding(CheckBox::textProperty, observableText);
		return this;
	}

	public GSCheckBox setText(String text) {
		addBoot(CheckBox::textProperty, text);
		return this;
	}

	public <M> GSCheckBox bindBidirectional(Function<M, Property<Boolean>> selectedProperty) {
		addBidirectionalBinding(CheckBox::selectedProperty, selectedProperty);
		return this;
	}
}
