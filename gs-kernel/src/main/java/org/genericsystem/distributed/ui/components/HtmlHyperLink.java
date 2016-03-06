package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import javafx.event.Event;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlDomNode;

public class HtmlHyperLink extends HtmlElement<HtmlHyperLink, HtmlDomNode> {

	public HtmlHyperLink(HtmlElement<?, ?> parent, String text) {
		super(parent, HtmlDomNode.class);
		addBoot(HtmlDomNode::getText, text);
	}

	public <M> HtmlHyperLink(HtmlElement<?, ?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M, T extends Event> HtmlHyperLink setAction(Consumer<M> consumer) {
		addActionBinding(HtmlDomNode::getActionProperty, consumer);
		return this;
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "a");
	}

}
