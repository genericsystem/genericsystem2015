package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag.HtmlDomNode;

public class GSOption extends GSTag {

	public GSOption(GSTag parent) {
		super(parent, "option");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}