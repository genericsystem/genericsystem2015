package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.property.Property;
import javafx.scene.control.TextField;

import org.genericsystem.ui.Element;

public class GSTextField extends GSRegion<GSTextField, TextField> {

	public <M> GSTextField(Element parent, Function<M, Property<String>> observableText) {
		super(parent, TextField.class);
		bindTextProperty(observableText);
	}

	public <M> GSTextField(Element parent, Function<M, Property<String>> observableText, Number size) {
		super(parent, TextField.class);
		bindTextProperty(observableText);
		setPrefWidth(size);
	}

	public <M> GSTextField(Element parent, Number size) {
		super(parent, TextField.class);
		setPrefWidth(size);
	}

	public <M> GSTextField(Element parent) {
		super(parent, TextField.class);
	}

	public <M> void bindTextProperty(Function<M, Property<String>> observableText) {
		addBidirectionalBinding(TextField::textProperty, observableText);
	}
}
