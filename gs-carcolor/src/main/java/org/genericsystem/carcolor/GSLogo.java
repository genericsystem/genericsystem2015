package org.genericsystem.carcolor;

import org.genericsystem.reactor.htmltag.HtmlImg;

import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;

import org.genericsystem.reactor.Tag;

public class GSLogo extends GSDiv {
	public GSLogo(Tag parent) {
		super(parent, FlexDirection.ROW);
		addStyle("flex", "0 1 auto");
		new HtmlImg(this, "logoTransp.png") {
			{
				addAttribute("alt", "logo");
				addStyle("height", "100%");
			}
		};
	}
}
