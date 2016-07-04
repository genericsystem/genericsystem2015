package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlP<M extends Model> extends Tag<M> {

	public HtmlP(Tag<?> parent) {
		super(parent, "p");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}