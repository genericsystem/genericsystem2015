package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

public class HtmlHeader extends GSTagImpl {

	public HtmlHeader() {
	}

	public HtmlHeader(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "header";
	}
}
