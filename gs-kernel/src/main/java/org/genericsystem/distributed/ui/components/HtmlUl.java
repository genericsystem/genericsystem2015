package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlUl extends HtmlElement<HtmlUl> {

	public HtmlUl(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode ul = new HtmlNode(getWebSocket());
		ul.getTag().set("ul");
		return ul;
	}

}
