package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

public class HtmlStrong extends HtmlElement {

	public HtmlStrong(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode strong = new HtmlNode(getWebSocket());
		strong.getTag().set("strong");
		return strong;
	}

}
