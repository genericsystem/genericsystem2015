package org.genericsystem.reactor.az2;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlLabel;

public class GSValueSubCell extends GSComposite {

	public GSValueSubCell(Tag parent, MetaTag metaTag) {
		super(parent, metaTag, ((GSComposite) parent).getDirection());
		new HtmlLabel(this).bindText();
	}
}