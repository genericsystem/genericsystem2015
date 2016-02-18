package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

public class HtmlSpan extends HtmlElement {

	public HtmlSpan(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode span = new HtmlNode(getWebSocket());
		span.getTag().set("span");
		return span;
	}

}
