package org.genericsystem.ui.components;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.scene.control.Label;

import org.genericsystem.todoKernel.Element;

public class GSLabel extends GSRegion<GSLabel, Label> {
	public GSLabel(Element<?> parent, String text) {
		super(parent, Label.class);
		setText(text);
	}

	public <M> GSLabel(Element<?> parent, Function<M, ObservableValue<String>> observableText) {
		super(parent, Label.class);
		setObservableTextProperty(observableText);
	}

	public GSLabel setText(String text) {
		addBoot(Label::textProperty, text);
		return this;
	}

	public <M> GSLabel setObservableTextProperty(Function<M, ObservableValue<String>> observableText) {
		addBinding(Label::textProperty, observableText);
		return this;
	}

}
