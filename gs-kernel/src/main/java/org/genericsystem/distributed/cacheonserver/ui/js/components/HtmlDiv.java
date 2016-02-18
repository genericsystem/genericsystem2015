package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

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
