package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlDiv extends HtmlElement<HtmlDiv, HtmlNode> {
	public HtmlDiv(HtmlElement<?, ?> parent) {
		super(parent, HtmlNode.class);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		return new HtmlNode(getWebSocket(), "div");
	}
}
