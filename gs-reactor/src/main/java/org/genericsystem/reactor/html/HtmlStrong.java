package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlElement;
import org.genericsystem.reactor.HtmlElement.HtmlDomNode;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlStrong<M extends Model> extends HtmlElement<M, HtmlDomNode> {

	public HtmlStrong(HtmlElement<?, ?> parent) {
		super(parent, "strong", HtmlDomNode.class);
	}

	@Override
	protected HtmlDomNode createNode(Object parent) {
		return new HtmlDomNode();
	}
}
