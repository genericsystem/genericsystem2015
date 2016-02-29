package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlDiv extends HtmlElement {
	public HtmlDiv(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode div = new HtmlNode(getWebSocket());
		div.getTag().set("div");
		return div;
	}
}
