package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlNodeInput;

public class HtmlInputText extends HtmlElement {

	public HtmlInputText(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeInput createNode(Object parent) {
		HtmlNodeInput input = new HtmlNodeInput(getWebSocket(), "text");
		input.getTag().set("input");
		return input;
	}

}
