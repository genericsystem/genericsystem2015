package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.Model;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel<M extends Model> extends Tag<M> {

	public HtmlLabel(Tag<?> parent) {
		super(parent, "label");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
