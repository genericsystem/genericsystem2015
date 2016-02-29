package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlH1 extends HtmlElement {

	public HtmlH1(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode h1 = new HtmlNode(getWebSocket());
		h1.getTag().set("h1");
		return h1;
	}

}
