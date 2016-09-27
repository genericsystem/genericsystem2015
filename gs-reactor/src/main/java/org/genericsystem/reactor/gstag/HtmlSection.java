package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

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
