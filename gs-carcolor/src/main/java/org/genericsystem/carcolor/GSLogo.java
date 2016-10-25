package org.genericsystem.carcolor;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gscomponents.FlexDirection;
import org.genericsystem.reactor.gscomponents.GSDiv;
import org.genericsystem.reactor.htmltag.HtmlImg;

public class GSLogo extends GSDiv {
	public GSLogo(Tag parent) {
		super(parent, FlexDirection.ROW);
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
