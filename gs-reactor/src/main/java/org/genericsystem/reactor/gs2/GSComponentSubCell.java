package org.genericsystem.reactor.gs2;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gstag.HtmlLabel;

public class GSComponentSubCell extends GSComposite {

	public GSComponentSubCell(Tag parent, MetaTag metaTag) {
		super(parent, metaTag);
		new HtmlLabel(this).bindText();
	}
}
