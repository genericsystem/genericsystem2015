package org.genericsystem.reactor.gs2;

public class GSCell extends GSComposite {

	public GSCell(GSRow parent, MetaTag metaTag) {
		super(parent, metaTag);
	}

	public static class GSFirstCell extends GSCell {

		public GSFirstCell(GSRow parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstRowFirstCell extends GSCell {

		public GSFirstRowFirstCell(GSRow parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}

	public static class GSFirstRowCell extends GSCell {

		public GSFirstRowCell(GSRow parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
