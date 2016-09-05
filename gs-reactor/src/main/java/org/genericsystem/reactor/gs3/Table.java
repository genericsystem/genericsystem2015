package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.Tag;
import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel;

public class Table extends GSDiv {

	public Table(Tag parent) {
		super(parent, FlexDirection.COLUMN);
	}

	@Parent(Table.class)
	public static class Row extends GSDiv {

		public Row(Tag parent) {
			super(parent);
		}
	}

	@Parent(Row.class)
	public static class RowName extends GSDiv {

		public RowName(Tag parent) {
			super(parent);
		}
	}

	@Parent(RowName.class)
	public static class RowNameDisplayer extends HtmlHyperLink {

		public RowNameDisplayer(Tag parent) {
			super(parent);
		}
	}

	@Parent(Row.class)
	public static class Cell extends GSDiv {

		public Cell(Tag parent) {
			super(parent);
		}
	}

	@Parent(Cell.class)
	public static class SubCell extends GSDiv {

		public SubCell(Tag parent) {
			super(parent);
		}
	}

	@Parent(SubCell.class)
	public static class ComponentSubCell extends GSDiv {

		public ComponentSubCell(Tag parent) {
			super(parent);
		}
	}

	@Parent(ComponentSubCell.class)
	public static class ComponentLabel extends HtmlLabel {

		public ComponentLabel(Tag parent) {
			super(parent);
		}
	}

	@Parent(SubCell.class)
	public static class BooleanValueSubCell extends GSDiv {

		public BooleanValueSubCell(Tag parent) {
			super(parent);
		}
	}

	@Parent(BooleanValueSubCell.class)
	public static class BooleanDisplayer extends GSCheckBoxDisplayer {

		public BooleanDisplayer(Tag parent) {
			super(parent);
		}
	}

	@Parent(SubCell.class)
	public static class ValueSubCell extends GSDiv {

		public ValueSubCell(Tag parent) {
			super(parent);
		}
	}

	@Parent(ValueSubCell.class)
	public static class ValueDisplayer extends HtmlLabel {

		public ValueDisplayer(Tag parent) {
			super(parent);
		}
	}
}
