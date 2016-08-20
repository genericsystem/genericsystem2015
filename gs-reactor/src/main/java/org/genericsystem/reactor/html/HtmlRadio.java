package org.genericsystem.reactor.html;

import org.genericsystem.reactor.HtmlDomNode.InputCheckHtmlDomNode;
import org.genericsystem.reactor.Model;
import org.genericsystem.reactor.Tag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlRadio<M extends Model> extends Tag<M> {

	public HtmlRadio(Tag<?> parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "radio");
	}
}
