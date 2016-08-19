package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.GSImg;

public class GSLogo extends GSImg {

	public GSLogo(GSTag parent) {
		super(parent);
		addAttribute("src", "logoTransp.png");
		addAttribute("alt", "logo");
		addStyle("height", "100%");
		addStyle("flex", "0 1 auto");
	}

}
	