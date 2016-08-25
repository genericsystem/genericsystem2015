package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

public class HtmlHeader extends GSTag{
	public HtmlHeader(GSTag parent) {
		super(parent, "header");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
