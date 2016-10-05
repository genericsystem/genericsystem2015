package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlDiv extends GSTagImpl {

	public HtmlDiv() {

	}

	public HtmlDiv(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "div";
	}
}
