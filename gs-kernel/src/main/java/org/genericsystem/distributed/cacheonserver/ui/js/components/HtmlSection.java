package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode;

public class HtmlSection extends HtmlElement {

	public HtmlSection(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode section = new HtmlNode(getWebSocket());
		section.getTag().set("section");
		return section;
	}
}
