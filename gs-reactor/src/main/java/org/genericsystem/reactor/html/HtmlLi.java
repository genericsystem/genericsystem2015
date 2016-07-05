package org.genericsystem.reactor.html;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLi<M extends ModelContext> extends Tag<M> {

	public HtmlLi(Tag<?> parent) {
		super(parent, "li");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
