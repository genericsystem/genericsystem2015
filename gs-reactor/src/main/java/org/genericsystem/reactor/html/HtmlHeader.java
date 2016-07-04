package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlHeader<M extends Model> extends Tag<M> {

	public HtmlHeader(Tag<?> parent) {
		super(parent, "header");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
