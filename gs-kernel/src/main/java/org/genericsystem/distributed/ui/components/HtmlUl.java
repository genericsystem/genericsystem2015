package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlUl extends HtmlElement<HtmlUl, HtmlNode> {

	public HtmlUl(HtmlElement<?, ?> parent) {
		super(parent, HtmlNode.class);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		return new HtmlNode(getWebSocket(), "ul");
	}

}
