package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLi<M extends Model> extends HtmlElement<M, HtmlDomNode> {

	public HtmlLi(HtmlElement<?, ?> parent) {
		super(parent, "li", HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode();
	}
}
