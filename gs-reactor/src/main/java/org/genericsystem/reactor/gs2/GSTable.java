package org.genericsystem.reactor.gs2;

import org.genericsystem.reactor.gs2.GSCell.GSFirstCell;
import org.genericsystem.reactor.gs2.GSCell.GSFirstRowCell;
import org.genericsystem.reactor.gs2.GSCell.GSFirstRowFirstCell;
import org.genericsystem.reactor.gs2.GSRow.GSFirstRow;
import org.genericsystem.reactor.model.ObservableListExtractor;

public class GSTable extends GSComposite {

	public static class TableMetaTag extends MetaTag {
		public TableMetaTag() {
			super(null, new MetaTag(GSRow.class, new MetaTag(GSCell.class)));
		}
	}

	public GSTable(GSComposite parent) {
		this(parent, new TableMetaTag());
	}

	public GSTable(GSComposite parent, MetaTag metaTag) {
		super(parent, metaTag);
		tags(GSRow.class).forEach(gsrow -> gsrow.forEach(ObservableListExtractor.SUBINSTANCES));
		tags(GSRow.class, GSCell.class).forEach(gsrow -> gsrow.forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES));
	}

	public static class GSFirstRowTableMetaTag extends MetaTag {
		public GSFirstRowTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSFirstRowCell.class)), new MetaTag(GSRow.class, new MetaTag(GSCell.class)));
		}
	}

	public class GSFirstRowTable extends GSTable {

		public GSFirstRowTable(GSComposite parent) {
			super(parent, new GSFirstRowTableMetaTag());
		}
	}

	public static class GSFirstColumnTableMetaTag extends MetaTag {
		public GSFirstColumnTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSRow.class, new MetaTag(GSFirstCell.class), new MetaTag(GSCell.class))));
		}
	}

	public class GSFirstColumnTable extends GSTable {

		public GSFirstColumnTable(GSComposite parent) {
			super(parent, new GSFirstColumnTableMetaTag());
		}
	}

	public static class GSFirstRowFirstColumnTableMetaTag extends MetaTag {
		public GSFirstRowFirstColumnTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSFirstRowFirstCell.class), new MetaTag(GSFirstRowCell.class)), new MetaTag(GSRow.class, new MetaTag(GSFirstCell.class), new MetaTag(GSCell.class)));
		}
	}

	public static class GSFirstRowFirstColumnTable extends GSTable {

		public GSFirstRowFirstColumnTable(GSComposite parent) {
			super(parent, new GSFirstRowFirstColumnTableMetaTag());
		}
	}

}
