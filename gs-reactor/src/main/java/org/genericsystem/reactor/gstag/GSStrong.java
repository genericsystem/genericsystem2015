package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSStrong extends GSTag {

	public GSStrong(GSTag parent) {
		super(parent, "strong");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}
}
