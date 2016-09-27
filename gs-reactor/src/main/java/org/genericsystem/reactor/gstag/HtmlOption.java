package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

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