package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

public class HtmlLi extends HtmlElement {

	public HtmlLi(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode li = new HtmlNode(getWebSocket());
		li.getTag().set("li");
		return li;
	}

}
