package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlH1 extends HtmlElement<HtmlH1> {

	public HtmlH1(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode h1 = new HtmlNode(getWebSocket());
		h1.getTag().set("h1");
		return h1;
	}

}
