package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlInputNode;

public class HtmlCheckBox extends HtmlElement {

	public HtmlCheckBox(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlInputNode createNode(Object parent) {
		HtmlInputNode h1 = new HtmlInputNode(getWebSocket(), "checkbox");
		h1.getTag().set("input");
		return h1;
	}

}
