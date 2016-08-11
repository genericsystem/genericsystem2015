package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

public class GSImg extends GSTag {

	public GSImg(GSTag parent) {
		super(parent, "img");
	}

	public GSImg(GSTag parent, String url) {
		super(parent, "img");
		addAttribute("src", url);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
