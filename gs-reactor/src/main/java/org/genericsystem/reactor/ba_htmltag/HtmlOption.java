package org.genericsystem.reactor.ba_htmltag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.ca_gscomponents.GSTagImpl;

public class HtmlOption extends GSTagImpl {

	public HtmlOption() {

	}

	public HtmlOption(Tag parent) {
		super(parent);
	}

	@Override
	public String getTag() {
		return "option";
	}
}