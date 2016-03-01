package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlLi extends HtmlElement<HtmlLi> {

	public HtmlLi(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode li = new HtmlNode(getWebSocket());
		li.getTag().set("li");
		return li;
	}

}
