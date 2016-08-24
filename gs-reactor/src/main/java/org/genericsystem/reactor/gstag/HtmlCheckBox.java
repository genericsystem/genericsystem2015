package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode.InputCheckHtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlCheckBox extends GSTag {

	public HtmlCheckBox(GSTag parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "checkbox");
	}
}
