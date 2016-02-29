package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

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
