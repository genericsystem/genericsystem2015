package org.genericsystem.reactor.html;

import org.genericsystem.reactor.Element;
import org.genericsystem.reactor.model.CompositeModel;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlSelect<M extends CompositeModel> extends Element<M> {

	public HtmlSelect(Element<?> parent) {
		super(parent, "select");
	}

	@Override
	protected SelectableHtmlDomNode createNode(String parentId) {
		return new SelectableHtmlDomNode(parentId);
	}
}
