package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlDomNode;

public class HtmlButton extends HtmlElement<HtmlButton, HtmlDomNode> {

	public HtmlButton(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "button");
	}

	public <M> HtmlButton setAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlDomNode::getActionProperty, applyOnModel);
		return this;
	}
}
