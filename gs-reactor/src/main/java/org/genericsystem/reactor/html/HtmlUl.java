package org.genericsystem.reactor.html;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlUl<M extends ModelContext> extends Tag<M> {

	public HtmlUl(Tag<?> parent) {
		super(parent, "ul");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
