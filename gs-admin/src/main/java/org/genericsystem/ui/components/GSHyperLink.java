package org.genericsystem.ui.components;

import java.util.function.Consumer;

import javafx.event.Event;
import javafx.scene.control.Hyperlink;

import org.genericsystem.ui.Element;

public class GSHyperLink extends GSRegion<GSHyperLink, Hyperlink> {

	public <M> GSHyperLink(Element parent, String text) {
		super(parent, Hyperlink.class);
		setText(text);
	}

	public <M> GSHyperLink(Element parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M, T extends Event> GSHyperLink setAction(Consumer<M> consumer) {
		addActionBinding(Hyperlink::onActionProperty, consumer);
		return this;
	}

	public GSHyperLink setText(String text) {
		addBoot(Hyperlink::textProperty, text);
		return this;
	}
}
