package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLi<M extends Model> extends Element<M> {

	public HtmlLi(Element<?> parent) {
		super(parent, "li");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
