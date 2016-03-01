package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlLabel extends HtmlElement<HtmlLabel> {

	public HtmlLabel(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode label = new HtmlNode(getWebSocket());
		label.getTag().set("label");
		return label;
	}

}
