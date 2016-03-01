package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode.HtmlNodeCheckBox;

public class HtmlCheckBox extends HtmlElement<HtmlCheckBox> {

	public HtmlCheckBox(HtmlElement<?> parent) {
		super(parent);
	}

	@Override
	protected HtmlNodeCheckBox createNode(Object parent) {
		HtmlNodeCheckBox checkBox = new HtmlNodeCheckBox(getWebSocket());
		checkBox.getTag().set("input");
		return checkBox;
	}

}
