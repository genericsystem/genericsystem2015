package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSH2 extends GSTag {

	public GSH2(GSTag parent) {
		super(parent, "h2");
	}

	public GSH2(GSTag parent, String text) {
		super(parent, "h2");
		setText(text);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
