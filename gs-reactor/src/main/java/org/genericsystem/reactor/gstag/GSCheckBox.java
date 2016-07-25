package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSCheckBox extends GSTag {

	public GSCheckBox(GSTag parent) {
		super(parent, "input");
	}

	@Override
	protected InputCheckHtmlDomNode createNode(String parentId) {
		return new InputCheckHtmlDomNode(parentId, "checkbox");
	}
}
