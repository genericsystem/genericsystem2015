package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlInputNode;

public class HtmlInputText extends HtmlElement {

	public HtmlInputText(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlInputNode createNode(Object parent) {
		HtmlInputNode input = new HtmlInputNode(getWebSocket(), "text");
		input.getTag().set("input");
		return input;
	}

}
