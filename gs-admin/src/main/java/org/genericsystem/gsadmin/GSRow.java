package org.genericsystem.gsadmin;

import javafx.geometry.Insets;
import javafx.scene.control.Button;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GSTable.GSTextCellTable;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;

public class GSRow extends GSHBox {

	public GSRow(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {
		GSHBox firstCell = new GSHBox(this).select(GenericRow::getFirstElement).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			new GSLabel(firstCell, Cell<String>::getObservableModel);
		}

		GSHBox secondCell = new GSHBox(this).select(Row::getSecondElement).setMinWidth(Table::getSecondColumnWidth).setPrefWidth(Table::getSecondColumnWidth).setMaxWidth(Table::getSecondColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			if (this instanceof GSTableCellRow) {
				GSVBox vb = new GSVBox(secondCell);
				new GSTextCellTable(vb).select(Cell<Table>::getObservableModel);// .include(new TextCellTableBuilder()::init);
			} else
				new GSLabel(secondCell, Cell<String>::getObservableModel);
		}

		GSHBox cells = new GSHBox(this).forEach(GenericRow::getElements).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			if (this instanceof GSTableCellRow) {
				GSVBox vb = new GSVBox(cells);
				new GSTextCellTable(vb).select(Cell<Table>::getObservableModel);
				// new GSVBox(cells).select(Cell<Table>::getObservableModel).include(new TextCellTableBuilder()::init);
			} else
				new GSLabel(cells, Cell<String>::getObservableModel);
		}

		GSHBox lastCell = new GSHBox(this).select(Row::getLastElement).setMinWidth(Table::getLastColumnWidth).setPrefWidth(Table::getLastColumnWidth).setMaxWidth(Table::getLastColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			new GSButton(lastCell, Cell<String>::getObservableModel).setAction(GenericRow::delete).addBoot(Button::paddingProperty, new Insets(2, 2, 2, 2));
		}
	}

	public static class GSTextCellRow extends GSRow {
		public GSTextCellRow(Element<?> parent) {
			super(parent);
			System.out.println("GSTextCellRow");
		}
	}

	public static class GSTextCellFirstRow extends GSTextCellRow {
		public GSTextCellFirstRow(Element<?> parent) {
			super(parent);
			System.out.println("GSTextCellFirstRow");
		}
	}

	public static final class GSTableCellRow extends GSRow {
		public GSTableCellRow(Element<?> parent) {
			super(parent);
			System.out.println("GSTableCellRow");
		}
	}
}