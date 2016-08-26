package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.gs.GSTag;

public class HtmlImg extends GSTag {

	public HtmlImg(GSTag parent) {
		super(parent, "img");
	}

	public HtmlImg(GSTag parent, String url) {
		super(parent, "img");
		addAttribute("src", url);
	}
}
