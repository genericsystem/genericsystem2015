package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.TagImpl;

public class HtmlImg extends TagImpl {

	public HtmlImg(Tag parent) {
		super(parent, "img");
	}

	public HtmlImg(Tag parent, String imageName) {
		super(parent, "img");
		addAttribute("src", imageName);
	}
}
