package org.genericsystem.reactor.gs2;

public class GSRow extends GSComposite {

	public GSRow(GSTable parent, MetaTag metaTag) {
		super(parent, metaTag);
	}

	public static class GSFirstRow extends GSRow {

		public GSFirstRow(GSTable parent, MetaTag metaTag) {
			super(parent, metaTag);
		}
	}
}
