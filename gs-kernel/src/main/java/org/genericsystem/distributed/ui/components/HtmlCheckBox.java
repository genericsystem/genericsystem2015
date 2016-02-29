package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeCheckBox;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeInput;

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
