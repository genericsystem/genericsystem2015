package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlButton extends HtmlElement {

	public HtmlButton(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode button = new HtmlNode(getWebSocket());
		button.getTag().set("button");
		return button;
	}
}
