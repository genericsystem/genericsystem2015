package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlDomNode;

public class HtmlSection extends HtmlElement<HtmlSection, HtmlDomNode> {

	public HtmlSection(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "section");
	}
}
