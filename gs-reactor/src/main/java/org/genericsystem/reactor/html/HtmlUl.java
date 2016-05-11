package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlUl<M extends Model> extends HtmlElement<M, HtmlUl<M>, HtmlDomNode> {

	public HtmlUl(HtmlElement<?, ?, ?> parent) {
		super(parent, HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode("ul");
	}

}
