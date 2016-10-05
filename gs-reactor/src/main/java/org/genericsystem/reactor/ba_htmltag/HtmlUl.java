package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

/**
 * @author Nicolas Feybesse
 *
 */
public class HtmlUl extends GSTagImpl {

	public HtmlUl() {

	}

	public HtmlUl(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "ul";
	}
}
