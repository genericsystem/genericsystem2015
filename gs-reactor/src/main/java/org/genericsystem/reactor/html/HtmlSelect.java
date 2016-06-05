package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.composite.CompositeModel;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlSelect<M extends CompositeModel> extends HtmlElement<M, HtmlDomNode> {

	public HtmlSelect(HtmlElement<?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("select");
	}
}
