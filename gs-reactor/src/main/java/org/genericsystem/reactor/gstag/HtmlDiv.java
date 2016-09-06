package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv extends TagImpl {

	public HtmlDiv() {
		super("div");
	}

	public HtmlDiv(Tag parent) {
		super(parent, "div");
	}
}
