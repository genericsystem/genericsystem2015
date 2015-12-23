package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.TextCell;

public interface CellCreation<T> {
	Cell<T> create(ObservableValue<T> observableString, ObservableValue<String> styleClass);

	public static interface TextCellCreation extends CellCreation<String> {
		@Override
		default TextCell create(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			return observableString != null ? new TextCell(observableString, styleClass) : null;
		}
	}

	public static interface ExtendedCellCreation extends CellCreation<Table> {
		@Override
		default ExtendedCell create(ObservableValue<Table> observableTable, ObservableValue<String> styleClass) {
			return new ExtendedCell(observableTable, styleClass);
		}
	}

}
