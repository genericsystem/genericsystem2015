package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

public class HtmlImg extends GSTagImpl {

	public HtmlImg(Tag parent) {
		super(parent, "img");
	}

	public HtmlImg(Tag parent, String imageName) {
		super(parent, "img");
		addAttribute("src", imageName);
	}
}
