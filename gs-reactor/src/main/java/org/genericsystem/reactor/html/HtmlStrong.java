package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlStrong<M extends Model> extends Element<M> {

	public HtmlStrong(Element<?> parent) {
		super(parent, "strong");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
