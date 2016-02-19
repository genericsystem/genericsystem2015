package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlInput;

public class HtmlCheckBox extends HtmlElement {

	public HtmlCheckBox(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlInput createNode(Object parent) {
		HtmlInput h1 = new HtmlInput(getWebSocket(), "checkbox");
		h1.getTag().set("input");
		return h1;
	}

}
