package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlElement;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlUl extends HtmlElement<HtmlUl, HtmlDomNode> {

	public HtmlUl(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "ul");
	}

}
