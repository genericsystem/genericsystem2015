package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlUl<M extends Model> extends Tag<M> {

	public HtmlUl(Tag<?> parent) {
		super(parent, "ul");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
