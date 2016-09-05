package org.genericsystem.reactor.gs2;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.gs.GSDiv;
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

	public GSTable(Tag parent) {
		this(parent, new TableMetaTag());
	}

	public GSTable(Tag parent, MetaTag metaTag) {
		super(parent, metaTag);
		tags(GSRow.class).forEach(gsrow -> {
			gsrow.forEach(ObservableListExtractor.SUBINSTANCES);
			gsrow.addStyle("flex", "1");
		});
		tags(GSRow.class, GSCell.class).forEach(gscell -> {
			gscell.forEach(ObservableListExtractor.ATTRIBUTES_OF_INSTANCES);
			gscell.addStyle("flex", "1");
			gscell.addStyle("flex-direction", ((GSDiv) gscell.getParent()).getReverseDirection().toString());
			gscell.addStyle("margin-bottom", "1px");
			gscell.addStyle("margin-right", "1px");
		});
	}

	public static class GSFirstRowTableMetaTag extends MetaTag {
		public GSFirstRowTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSFirstRowCell.class)), new MetaTag(GSRow.class, new MetaTag(GSCell.class)));
		}
	}

	public class GSFirstRowTable extends GSTable {

		public GSFirstRowTable(Tag parent) {
			super(parent, new GSFirstRowTableMetaTag());
		}
	}

	public static class GSFirstColumnTableMetaTag extends MetaTag {
		public GSFirstColumnTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSRow.class, new MetaTag(GSFirstCell.class), new MetaTag(GSCell.class))));
		}
	}

	public class GSFirstColumnTable extends GSTable {

		public GSFirstColumnTable(Tag parent) {
			super(parent, new GSFirstColumnTableMetaTag());
		}
	}

	public static class GSFirstRowFirstColumnTableMetaTag extends MetaTag {
		public GSFirstRowFirstColumnTableMetaTag() {
			super(null, new MetaTag(GSFirstRow.class, new MetaTag(GSFirstRowFirstCell.class), new MetaTag(GSFirstRowCell.class)), new MetaTag(GSRow.class, new MetaTag(GSFirstCell.class), new MetaTag(GSCell.class)));
		}
	}

	public static class GSFirstRowFirstColumnTable extends GSTable {

		public GSFirstRowFirstColumnTable(Tag parent) {
			super(parent, new GSFirstRowFirstColumnTableMetaTag());
		}
	}

	public static class GSSubSubCellTableMetaTag extends MetaTag {

		public GSSubSubCellTableMetaTag() {
			super(null, new MetaTag(GSRow.class, new MetaTag(GSCell.class, new MetaTag(GSSubCell.class, new MetaTag(GSValueSubCell.class), new MetaTag(GSComponentSubCell.class)))));
		}
	}

	public static class GSSubSubCellTable extends GSTable {

		public GSSubSubCellTable(Tag parent) {
			super(parent, new GSSubSubCellTableMetaTag());
			tags(GSRow.class, GSCell.class, GSSubCell.class).forEach(gssubcell -> {
				gssubcell.forEach(ObservableListExtractor.HOLDERS);
				gssubcell.addStyle("flex", "1");
			});
			tags(GSRow.class, GSCell.class, GSSubCell.class, GSComponentSubCell.class).forEach(t -> {
				t.forEach(gs -> ObservableListExtractor.COMPONENTS.apply(gs).filtered(g -> !g.equals(gs[2])));
				t.addStyle("flex", "1");
				t.addStyle("justify-content", "center");
				t.addStyle("align-items", "center");
				t.addStyle("margin-bottom", "1px");
				t.addStyle("margin-right", "1px");
				t.addStyle("background-color", "#e5e400");
			});
			tags(GSRow.class, GSCell.class, GSSubCell.class, GSValueSubCell.class).forEach(t -> {
				t.select(gs -> gs[1].getComponents().size() == 1 ? gs[0] : null);
				t.addStyle("justify-content", "center");
				t.addStyle("align-items", "center");
				t.addStyle("flex", "1");
				t.addStyle("background-color", "#e5e400");
			});
		}
	}
}
