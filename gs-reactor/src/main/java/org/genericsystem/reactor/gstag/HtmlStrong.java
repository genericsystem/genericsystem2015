package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlStrong extends GSTag {

	public HtmlStrong(GSTag parent) {
		super(parent, "strong");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
