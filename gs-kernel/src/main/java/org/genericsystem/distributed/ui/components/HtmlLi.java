package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlLi extends HtmlElement<HtmlLi, HtmlNode> {

	public HtmlLi(HtmlElement<?, ?> parent) {
		super(parent, HtmlNode.class);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		return new HtmlNode(getWebSocket(), "li");
	}
}
