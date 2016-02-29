package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlNode;

public class HtmlFooter extends HtmlElement {

	public HtmlFooter(HtmlElement parent) {
		super(parent);
		// TODO Auto-generated constructor stub
	}

	@Override
	protected HtmlNode createNode(Object parent) {
		HtmlNode footer = new HtmlNode(getWebSocket());
		footer.getTag().set("footer");
		return footer;
	}

}
