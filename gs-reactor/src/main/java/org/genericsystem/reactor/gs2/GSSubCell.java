package org.genericsystem.reactor.gs2;

public class GSSubCell extends GSComposite {

	public GSSubCell(GSCell parent, MetaTag metaTag) {
		super(parent, metaTag);
	}

	public static class GSFirstSubCell extends GSSubCell {

		public GSFirstSubCell(GSCell parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstCellFirstSubCell extends GSSubCell {

		public GSFirstCellFirstSubCell(GSCell parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstCellSubCell extends GSSubCell {

		public GSFirstCellSubCell(GSCell parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
