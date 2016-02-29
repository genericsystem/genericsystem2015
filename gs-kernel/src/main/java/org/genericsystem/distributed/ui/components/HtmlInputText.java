package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeInput;

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
