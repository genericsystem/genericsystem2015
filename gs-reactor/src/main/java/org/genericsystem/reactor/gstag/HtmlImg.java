package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;

public class HtmlImg extends Tag {

	public HtmlImg(Tag parent) {
		super(parent, "img");
	}

	public HtmlImg(Tag parent, String url) {
		super(parent, "img");
		addAttribute("src", url);
	}
}
