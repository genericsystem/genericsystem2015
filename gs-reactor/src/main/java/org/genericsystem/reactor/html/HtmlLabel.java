package org.genericsystem.reactor.html;

import org.genericsystem.reactor.ModelContext;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlLabel<M extends ModelContext> extends Tag<M> {

	public HtmlLabel(Tag<?> parent) {
		super(parent, "label");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
