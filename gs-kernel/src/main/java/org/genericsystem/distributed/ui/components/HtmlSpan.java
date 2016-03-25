package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlDomNode;
import org.genericsystem.distributed.ui.HtmlElement;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlSpan extends HtmlElement<HtmlSpan, HtmlDomNode> {

	public HtmlSpan(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode(getWebSocket(), "span");
	}

}
