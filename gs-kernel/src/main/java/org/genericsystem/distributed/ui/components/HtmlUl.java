package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlUl extends HtmlElement {

	public HtmlUl(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode ul = new HtmlNode(getWebSocket());
		ul.getTag().set("ul");
		return ul;
	}

}
