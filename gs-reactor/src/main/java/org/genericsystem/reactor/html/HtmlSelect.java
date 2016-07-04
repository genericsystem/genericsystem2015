package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.model.GenericModel;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlSelect<M extends GenericModel> extends Tag<M> {

	public HtmlSelect(Tag<?> parent) {
		super(parent, "select");
	}

	@Override
	protected SelectableHtmlDomNode createNode(String parentId) {
		return new SelectableHtmlDomNode(parentId);
	}
}
