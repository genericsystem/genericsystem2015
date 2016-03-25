package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlElement;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlP extends HtmlElement<HtmlP, HtmlDomNode> {

	public HtmlP(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "p");
	}

}