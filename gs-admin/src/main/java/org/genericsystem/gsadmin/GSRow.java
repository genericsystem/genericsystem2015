package org.genericsystem.gsadmin;

import javafx.geometry.Insets;
import javafx.scene.control.Button;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GSTable.GSTextCellTable;
import org.genericsystem.ui.Element;
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
			createGSCell(cells);
		}

		GSHBox lastCell = new GSHBox(this).select(Row::getLastElement).setMinWidth(Cell<Generic>::getLastColumnCellsWidth).setPrefWidth(Cell<Generic>::getLastColumnCellsWidth).setMaxWidth(Cell<Generic>::getLastColumnCellsWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		{
			createLastCell(lastCell);
		}

	}

	protected abstract Element<?> createGSCell(Element<?> parent);

	protected Element<?> createFirstCell(Element<?> parent) {
		return new GSLabel(parent, Cell<String>::getObservableModel);
	}

	protected Element<?> createLastCell(Element<?> parent) {
		return new GSButton(parent, Cell<String>::getObservableModel).setAction(GenericRow::delete).addBoot(Button::paddingProperty, new Insets(2, 2, 2, 2));
	}

	public static class GSEditableCellRow extends GSRow {

		public GSEditableCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createGSCell(Element<?> parent) {
			return new GSTextField(parent, Cell<Generic>::getName);
		}

		@Override
		protected Element<?> createFirstCell(Element<?> parent) {
			return new GSTextCellTable(parent).select(Cell<Table>::getObservableModel);
		}

	}

	public static class GSTextCellRow extends GSRow {
		public GSTextCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createGSCell(Element<?> parent) {
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

	public static final class GSTableCellRow extends GSRow {
		public GSTableCellRow(Element<?> parent) {
			super(parent);
		}

		@Override
		protected Element<?> createGSCell(Element<?> parent) {
			return new GSTextCellTable(parent).select(Cell<Table>::getObservableModel);
		}

		@Override
		protected Element<?> createFirstCell(Element<?> parent) {
			return createGSCell(parent);
		}
	}
}