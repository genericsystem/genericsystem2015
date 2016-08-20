package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlSpan<M extends Model> extends Tag<M> {

	public HtmlSpan(Tag<?> parent) {
		super(parent, "span");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
