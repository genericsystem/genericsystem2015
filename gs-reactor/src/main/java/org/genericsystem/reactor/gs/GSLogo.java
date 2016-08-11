package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.gstag.GSImg;

public class GSLogo extends GSImg {

	public GSLogo(GSTag parent) {
		super(parent);
		addAttribute("src", "logo.png");
		addAttribute("alt", "logo");
		addStyle("max-width", "80px");
		addStyle("height", "60px");
	}

}
