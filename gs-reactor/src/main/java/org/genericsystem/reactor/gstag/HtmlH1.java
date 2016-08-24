package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.HtmlDomNode;
import org.genericsystem.reactor.gs.GSTag;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlH1 extends GSTag {

	public HtmlH1(GSTag parent) {
		super(parent, "h1");
	}

	public HtmlH1(GSTag parent, String text) {
		super(parent, "h1");
		setText(text);
	}

	@Override
	protected HtmlDomNode createNode(String parentId) {
		return new HtmlDomNode(parentId);
	}

}
