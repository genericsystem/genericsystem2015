package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import javafx.event.Event;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlHyperLink extends HtmlElement<HtmlHyperLink, HtmlNode> {

	public HtmlHyperLink(HtmlElement<?, ?> parent, String text) {
		super(parent, HtmlNode.class);
		addBoot(HtmlNode::getText, text);
	}

	public <M> HtmlHyperLink(HtmlElement<?, ?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M, T extends Event> HtmlHyperLink setAction(Consumer<M> consumer) {
		addActionBinding(HtmlNode::getActionProperty, consumer);
		return this;
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		return new HtmlNode(getWebSocket(), "a");
	}

}
