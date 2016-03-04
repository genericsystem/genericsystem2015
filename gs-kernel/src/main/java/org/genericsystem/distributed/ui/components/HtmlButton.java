package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlButton extends HtmlElement<HtmlButton, HtmlNode> {

	public HtmlButton(HtmlElement<?, ?> parent) {
		super(parent, HtmlNode.class);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		return new HtmlNode(getWebSocket(), "button");
	}

	public <M> HtmlButton setAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
