package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlP extends HtmlElement {

	public HtmlP(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode p = new HtmlNode(getWebSocket());
		p.getTag().set("p");
		return p;
	}

}