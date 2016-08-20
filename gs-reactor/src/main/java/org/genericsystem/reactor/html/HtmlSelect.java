package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlDomNode.SelectableHtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public abstract class HtmlSelect<M extends Model> extends Tag<M> {

	public HtmlSelect(Tag<?> parent) {
		super(parent, "select");
	}

	@Override
	protected SelectableHtmlDomNode createNode(String parentId) {
		return new SelectableHtmlDomNode(parentId);
	}
}
