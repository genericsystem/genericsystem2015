package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import javafx.event.Event;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlHyperLink extends HtmlElement<HtmlHyperLink> {

	public HtmlHyperLink(HtmlElement<?> parent, String text) {
		super(parent);
		addBoot(HtmlNode::getText, text);
	}

	public <M> HtmlHyperLink(HtmlElement<?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M, T extends Event> HtmlHyperLink setAction(Consumer<M> consumer) {
		return addActionBinding(HtmlNode::getActionProperty, consumer);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode link = new HtmlNode(getWebSocket());
		link.getTag().set("a");
		return link;
	}

}
