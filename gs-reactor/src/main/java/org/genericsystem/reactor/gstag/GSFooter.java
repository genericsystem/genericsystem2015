package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSFooter extends GSTag {

	public GSFooter(GSTag parent) {
		super(parent, "footer");
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
