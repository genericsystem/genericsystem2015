package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv<M extends Model> extends HtmlElement<M, HtmlDomNode> {
	public HtmlDiv(HtmlElement<?, ?> parent) {
		super(parent, "div", HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode();
	}
}
