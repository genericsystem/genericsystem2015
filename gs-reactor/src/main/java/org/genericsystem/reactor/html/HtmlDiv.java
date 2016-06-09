package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv<M extends Model> extends Element<M> {
	public HtmlDiv(Element<?> parent) {
		super(parent, "div");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
