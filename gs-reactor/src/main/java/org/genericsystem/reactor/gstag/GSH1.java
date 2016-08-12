package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class GSH1 extends GSTag {

	public GSH1(GSTag parent) {
		super(parent, "h1");
	}

	public GSH1(GSTag parent, String text) {
		super(parent, "h1");
		setText(this, text);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
