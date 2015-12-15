package org.genericsystem.ui.components;

import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.event.Event;
import javafx.scene.control.Button;

import org.genericsystem.ui.Binding;
import org.genericsystem.ui.Element;

public class GSButton extends GSRegion<GSButton, Button> {

	public GSButton(Element parent, String text) {
		super(parent, Button.class);
		setText(text);
	}

	public <M> GSButton(Element parent, Function<M, ObservableValue<String>> observableText) {
		super(parent, Button.class);
		setObservableTextProperty(observableText);
	}

	public <M> GSButton(Element parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M> GSButton(Element parent, Function<M, ObservableValue<String>> observableText, Consumer<M> action) {
		this(parent, observableText);
		setAction(action);
	}

	// public <SUPERMODEL, M> GSButton(Element parent, String text, BiConsumer<SUPERMODEL, M> action) {
	// this(parent, text);
	// setMetaAction(action);
	// }

	public <M, T extends Event> GSButton setAction(Consumer<M> action) {
		bindings.add(Binding.bindAction(Button::onActionProperty, action));
		return this;
	}

	public <SUPERMODEL, M, T extends Event> GSButton setMetaAction(BiConsumer<SUPERMODEL, M> action) {
		bindings.add(Binding.bindMetaAction(Button::onActionProperty, action));
		return this;
	}

	public GSButton setText(String text) {
		addBoot(Button::textProperty, text);
		return this;
	}

	public <M> GSButton setObservableTextProperty(Function<M, ObservableValue<String>> observableText) {
		addBinding(Button::textProperty, observableText);
		return this;
	}
}
