package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlInput;

public class HtmlInputText extends HtmlElement {

	public HtmlInputText(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlInput createNode(Object parent) {
		HtmlInput input = new HtmlInput(getWebSocket(), "text");
		input.getTag().set("input");
		return input;
	}

}
