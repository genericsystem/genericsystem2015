package org.genericsystem.reactor.gs;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlImg;

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
