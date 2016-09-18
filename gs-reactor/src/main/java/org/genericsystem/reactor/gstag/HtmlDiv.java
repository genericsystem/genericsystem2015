package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv extends GSTagImpl {

	public HtmlDiv() {
		super("div");
	}

	public HtmlDiv(Tag parent) {
		super(parent, "div");
	}
}
