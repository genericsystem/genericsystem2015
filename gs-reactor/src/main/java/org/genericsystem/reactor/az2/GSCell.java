package org.genericsystem.reactor.az2;

import org.genericsystem.reactor.Tag;

public class GSCell extends GSComposite {

	public GSCell(Tag parent, MetaTag metaTag) {
		super(parent, metaTag, ((GSComposite) parent).getReverseDirection());
	}

	public static class GSFirstCell extends GSCell {

		public GSFirstCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstRowFirstCell extends GSCell {

		public GSFirstRowFirstCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstRowCell extends GSCell {

		public GSFirstRowCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
