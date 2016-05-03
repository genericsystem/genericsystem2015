package org.genericsystem.gsadmin;

import javafx.geometry.Insets;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Element;
import org.genericsystem.gsadmin.GSTable.GSEditTableCell;
import org.genericsystem.gsadmin.GSTable.GSTextCellTable;
import org.genericsystem.ui.components.GSButton;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSTextField;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;

public abstract class GSRow extends GSHBox {

	public GSRow(Element<?> parent) {
		super(parent);
	}

	@Override
	protected void initChildren() {

		GSHBox firstCell = new GSHBox(this).setStyleClass(Cell<Generic>::getStyleClass).select(Row::getFirstElement).setMinWidth(Cell<Generic>::getFirstColumnCellsWidth).setPrefWidth(Cell<Generic>::getFirstColumnCellsWidth)
				.setMaxWidth(Cell<Generic>::getFirstColumnCellsWidth).setStyleClass(Cell<Generic>::getStyleClass);
		{
			createFirstCell(firstCell);
		}

		GSHBox cells = new GSHBox(this).forEach(GenericRow::getElements).setMinWidth(Cell<Generic>::getColumnCellsWidth).setMaxWidth(Cell<Generic>::getColumnCellsWidth).setPrefWidth(Cell<Generic>::getColumnCellsWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		{
			createCells(cells);
		}

		GSHBox lastCell = new GSHBox(this).select(Row::getLastElement).setMinWidth(Cell<Generic>::getLastColumnCellsWidth).setPrefWidth(Cell<Generic>::getLastColumnCellsWidth).setMaxWidth(Cell<Generic>::getLastColumnCellsWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		{
			createLastCell(lastCell);
		}

	}

	protected abstract Element<?> createCells(Element<?> parent);

	protected Element<?> createFirstCell(Element<?> parent) {
		return new GSLabel(parent, Cell<String>::getObservableModel);
	}

	protected Element<?> createLastCell(Element<?> parent) {
		return new GSButton(parent, Cell<String>::getObservableModel).setAction(Cell<Generic>::delete).setPadding(new Insets(2, 2, 2, 2));
	}

	public static class GSTextCellRow extends GSRow {
		public GSTextCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createCells(Element<?> parent) {
			return new GSLabel(parent, Cell<String>::getObservableModel);
		}
	}

	public static class GSTextCellFirstRow extends GSTextCellRow {
		public GSTextCellFirstRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createLastCell(Element<?> parent) {
			// TODO Auto-generated method stub
			return new GSLabel(parent, Cell<String>::getObservableModel);
		}
	}

	public static class GSTableCellRow extends GSRow {
		public GSTableCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createCells(Element<?> parent) {
			return new GSTextCellTable(parent).select(Cell<Table>::getObservableModel);
		}

		@Override
		protected Element<?> createFirstCell(Element<?> parent) {
			return createCells(parent);
		}
	}

	public static class GSEditableCellTableRow extends GSRow {

		public GSEditableCellTableRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createCells(Element<?> parent) {
			return new GSEditTableCell(parent).select(Cell<Table>::getObservableModel);
		}

		@Override
		protected Element<?> createFirstCell(Element<?> parent) {
			return new GSTextCellTable(parent).select(Cell<Table>::getObservableModel);
		}
	}

	public static class GSEditableCellRow extends GSRow {

		public GSEditableCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createCells(Element<?> parent) {
			return null;
		}

		@Override
		protected Element<?> createFirstCell(Element<?> parent) {
			GSHBox cellPanel = new GSHBox(parent).setSpacing(2);
			new GSTextField(cellPanel, Cell<String>::getValue).bindTextProperty(Cell<String>::getNewValModel);
			new GSButton(cellPanel, "Update").setAction(Cell<Generic>::update).setPadding(new Insets(5, 2, 5, 2));
			return cellPanel;
		}
	}
}