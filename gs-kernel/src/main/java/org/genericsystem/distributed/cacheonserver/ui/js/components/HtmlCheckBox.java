package org.genericsystem.distributed.cacheonserver.ui.js.components;

import org.genericsystem.distributed.cacheonserver.ui.js.HtmlElement;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlNodeCheckBox;
import org.genericsystem.distributed.cacheonserver.ui.js.HtmlNode.HtmlNodeInput;

public class HtmlCheckBox extends HtmlElement {

	public HtmlCheckBox(HtmlElement parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeCheckBox createNode(Object parent) {
		HtmlNodeCheckBox checkBox = new HtmlNodeCheckBox(getWebSocket());
		checkBox.getTag().set("input");
		return checkBox;
	}

}
