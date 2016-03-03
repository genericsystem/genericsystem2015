package org.genericsystem.distributed.ui.components;

import java.util.function.Consumer;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlButton extends HtmlElement<HtmlButton> {

	public HtmlButton(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode button = new HtmlNode(getWebSocket());
		button.getTag().set("button");
		return button;
	}

	public <M> HtmlButton setAction(Consumer<M> applyOnModel) {
		addActionBinding(HtmlNode::getActionProperty, applyOnModel);
		return this;
	}
}
