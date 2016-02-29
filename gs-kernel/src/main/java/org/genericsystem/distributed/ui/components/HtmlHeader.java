package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlHeader extends HtmlElement {

	public HtmlHeader(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode header = new HtmlNode(getWebSocket());
		header.getTag().set("header");
		return header;
	}
}
