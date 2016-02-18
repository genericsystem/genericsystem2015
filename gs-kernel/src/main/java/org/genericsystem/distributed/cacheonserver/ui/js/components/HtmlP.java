package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

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