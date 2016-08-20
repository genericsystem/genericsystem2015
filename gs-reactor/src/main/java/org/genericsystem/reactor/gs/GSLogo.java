package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.GSImg;

public class GSLogo extends GSSection {

	public GSLogo(GSTag parent) {
		super(parent, FlexDirection.ROW);
		new GSImg(this) {
			{
				addAttribute("src", "logoTransp.png");
				addAttribute("alt", "logo");
				addStyle("height", "100%");
				addStyle("flex", "0 1 auto");
			}
		};
	}

}
