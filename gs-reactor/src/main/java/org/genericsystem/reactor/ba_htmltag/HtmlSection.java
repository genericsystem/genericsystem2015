package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

public class HtmlSection extends GSTagImpl {

	public HtmlSection() {

	}

	public HtmlSection(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "section";
	}
}
