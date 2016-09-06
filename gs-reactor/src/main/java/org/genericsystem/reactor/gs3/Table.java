package org.genericsystem.reactor.gs3;

import org.genericsystem.reactor.annotations.Parent;
import org.genericsystem.reactor.gs.FlexDirection;
import org.genericsystem.reactor.gs.GSCheckBoxWithValue.GSCheckBoxDisplayer;
import org.genericsystem.reactor.gs.GSDiv;
import org.genericsystem.reactor.gstag.HtmlHyperLink;
import org.genericsystem.reactor.gstag.HtmlLabel;

public class Table extends GSDiv {

	public Table() {
		super(FlexDirection.COLUMN);
	}

	@Parent(Table.class)
	public static class Row extends GSDiv {
	}

	@Parent(Row.class)
	public static class RowName extends GSDiv {
	}

	@Parent(RowName.class)
	public static class RowNameDisplayer extends HtmlHyperLink {
	}

	@Parent(Row.class)
	public static class Cell extends GSDiv {
	}

	@Parent(Cell.class)
	public static class SubCell extends GSDiv {

		public SubCell() {
			addStyle("border", "1px solid blue");
		}
	}

	@Parent(Cell.class)
	public static class SubCell2 extends SubCell {

		public SubCell2() {
			addStyle("border", "1px solid red");
		}
	}

	@Parent(SubCell.class)
	public static class ComponentSubCell extends GSDiv {
	}

	@Parent(ComponentSubCell.class)
	public static class ComponentLabel extends HtmlLabel {
	}

	@Parent(SubCell.class)
	public static class BooleanValueSubCell extends GSDiv {
	}

	@Parent(BooleanValueSubCell.class)
	public static class BooleanDisplayer extends GSCheckBoxDisplayer {
	}

	@Parent(SubCell.class)
	public static class ValueSubCell extends GSDiv {
	}

	@Parent(ValueSubCell.class)
	public static class ValueDisplayer extends HtmlLabel {
	}
}
