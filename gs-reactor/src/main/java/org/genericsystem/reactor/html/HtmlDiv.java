package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv<M extends Model> extends Tag<M> {
	public HtmlDiv(Tag<?> parent) {
		super(parent, "div");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
