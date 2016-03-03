package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlStrong extends HtmlElement<HtmlStrong> {

	public HtmlStrong(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode strong = new HtmlNode(getWebSocket());
		strong.getTag().set("strong");
		return strong;
	}
}
