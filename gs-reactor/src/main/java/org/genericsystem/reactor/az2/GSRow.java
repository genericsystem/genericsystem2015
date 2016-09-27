package org.genericsystem.reactor.az2;

import org.genericsystem.reactor.Tag;

public class GSRow extends GSComposite {

	public GSRow(Tag parent, MetaTag metaTag) {
		super(parent, metaTag, ((GSComposite) parent).getReverseDirection());
	}

	public static class GSFirstRow extends GSRow {

		public GSFirstRow(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
