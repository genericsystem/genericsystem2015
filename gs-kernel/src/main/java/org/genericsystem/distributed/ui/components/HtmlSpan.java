package org.genericsystem.distributed.ui.components;

import org.genericsystem.distributed.ui.HtmlElement;
import org.genericsystem.distributed.ui.HtmlElement.HtmlDomNode;

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
		return new HtmlDomNode("span");
	}

}
