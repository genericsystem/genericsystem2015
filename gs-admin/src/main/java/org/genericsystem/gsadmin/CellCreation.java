package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.FirstRowFirstCell;

public interface CellCreation {
	default Cell create(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel tableModel) {
		return new Cell(observableString, styleClass);
	}

	public static interface FirstRowFirstCellCreation extends CellCreation {
		@Override
		default Cell create(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel subTableModel) {
			return observableString != null ? new FirstRowFirstCell(observableString, styleClass) : null;
		}
	}

	public static interface RowFirstCellCreation extends CellCreation {
		@Override
		default Cell create(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel subTableModel) {
			return observableString != null ? new Cell(observableString, styleClass) : null;
		}
	}

	public static interface FirstCellCreation extends CellCreation {
		@Override
		default Cell create(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel subTableModel) {
			return new Cell(observableString, styleClass);
		}
	}

	public static interface ExtendedCellCreation extends CellCreation {
		@Override
		default Cell create(ObservableValue<String> observableString, ObservableValue<String> styleClass, TableModel subTableModel) {
			return new ExtendedCell(observableString, styleClass, subTableModel);
		}
	}

}
