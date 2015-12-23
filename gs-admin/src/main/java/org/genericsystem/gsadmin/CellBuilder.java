package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.TextCell;

public interface CellBuilder<T> {
	Cell<T> build(ObservableValue<T> observableString, ObservableValue<String> styleClass);

	public static interface TextCellBuilder extends CellBuilder<String> {
		@Override
		default TextCell build(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			return observableString != null ? new TextCell(observableString, styleClass) : null;
		}
	}

	public static interface TableCellBuilder extends CellBuilder<Table> {
		@Override
		default ExtendedCell build(ObservableValue<Table> observableTable, ObservableValue<String> styleClass) {
			return observableTable != null ? new ExtendedCell(observableTable, styleClass) : null;
		}
	}

}
