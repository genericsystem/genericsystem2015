package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlFooter extends GSTag {

	public HtmlFooter(GSTag parent) {
		super(parent, "footer");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
