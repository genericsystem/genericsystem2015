package org.genericsystem.reactor.gs2;

import org.genericsystem.reactor.Tag;

public class GSSubCell extends GSComposite {

	public GSSubCell(Tag parent, MetaTag metaTag) {
		super(parent, metaTag, ((GSComposite) parent).getReverseDirection());
	}

	public static class GSFirstSubCell extends GSSubCell {

		public GSFirstSubCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstCellFirstSubCell extends GSSubCell {

		public GSFirstCellFirstSubCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstCellSubCell extends GSSubCell {

		public GSFirstCellSubCell(Tag parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
