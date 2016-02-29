package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

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
