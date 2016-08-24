package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

public class HtmlOption extends GSTag {

	public HtmlOption(GSTag parent) {
		super(parent, "option");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}