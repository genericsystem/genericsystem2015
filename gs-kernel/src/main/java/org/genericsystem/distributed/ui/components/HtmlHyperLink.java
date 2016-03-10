package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;
import javafx.event.Event;
import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlDomNode.ActionHtmlNode;
import org.genericsystem.distributed.ui.HtmlElement;

public class HtmlHyperLink extends HtmlElement<HtmlHyperLink, ActionHtmlNode> {

	public HtmlHyperLink(HtmlElement<?, ?> parent, String text) {
		super(parent, ActionHtmlNode.class);
		addBoot(HtmlDomNode::getText, text);
	}

	public <M> HtmlHyperLink(HtmlElement<?, ?> parent, String text, Consumer<M> action) {
		this(parent, text);
		setAction(action);
	}

	public <M, T extends Event> HtmlHyperLink setAction(Consumer<M> consumer) {
		addActionBinding(ActionHtmlNode::getActionProperty, consumer);
		return this;
	}

	@Override
	protected ActionHtmlNode createNode(Object parent) {
		return new ActionHtmlNode(getWebSocket(), "a");
	}

}
