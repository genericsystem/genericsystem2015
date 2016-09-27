package org.genericsystem.reactor.gstag;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.az.GSTagImpl;

public class HtmlImg extends GSTagImpl {

	public HtmlImg() {

	}

	public HtmlImg(Tag parent) {
		super(parent);
	}

	public HtmlImg(Tag parent, String imageName) {
		super(parent);
		addAttribute("src", imageName);
	}

	@Override
	public String getTag() {
		return "img";
	}
}
