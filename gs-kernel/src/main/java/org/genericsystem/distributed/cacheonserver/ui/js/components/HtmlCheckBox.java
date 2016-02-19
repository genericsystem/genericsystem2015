package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlNodeInput;

public class HtmlCheckBox extends HtmlElement {

	public HtmlCheckBox(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeInput createNode(Object parent) {
		HtmlNodeInput h1 = new HtmlNodeInput(getWebSocket(), "checkbox");
		h1.getTag().set("input");
		return h1;
	}

}
