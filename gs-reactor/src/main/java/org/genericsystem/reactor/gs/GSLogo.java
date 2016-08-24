package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.HtmlImg;

public class GSLogo extends GSSection {

	public GSLogo(GSTag parent) {
		super(parent, FlexDirection.ROW);
		new HtmlImg(this) {
			{
				addAttribute("src", "logoTransp.png");
				addAttribute("alt", "logo");
				addStyle("height", "100%");
				addStyle("flex", "0 1 auto");
			}
		};
	}

}
