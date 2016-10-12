package org.genericsystem.reactor.gscomponents;

import org.genericsystem.reactor.htmltag.HtmlImg;

import org.genericsystem.reactor.Tag;

public class GSLogo extends GSDiv {

	public GSLogo(Tag parent) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "0 1 auto");
		addStyle("align-items", "center");
		new HtmlImg(this, "logoTransp.png") {
			{
				addAttribute("alt", "logo");
				addStyle("height", "auto");
				addStyle("width", "150px");
			}
		};
	}

}
