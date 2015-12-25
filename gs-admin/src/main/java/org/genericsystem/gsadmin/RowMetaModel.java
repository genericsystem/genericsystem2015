package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.TableStyle;

class RowMetaModel<COL, U, T> {
	final ObservableValue<String> firstColumnString;
	final Function<COL, ObservableValue<U>> columnExtractor;
	final TableStyle tableStyle;
	// final ObservableValue<String> firstElementStyle;
	// final ObservableValue<String> elementStyle;
	final ObservableList<COL> columns;

	RowMetaModel(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<U>> columnExtractor, TableStyle tableStyle/* , ObservableValue<String> firstElementStyle, ObservableValue<String> elementStyle */) {
		this.firstColumnString = firstColumnString;
		this.columnExtractor = columnExtractor;
		this.tableStyle = tableStyle;
		// this.firstElementStyle = firstElementStyle;
		// this.elementStyle = elementStyle;
		this.columns = columns;
	}

	public ObservableValue<String> getFirstColumnString() {
		return firstColumnString;
	}

	public TableStyle getTableStyle() {
		return tableStyle;
	}

	// public ObservableValue<String> getFirstElementStyle() {
	// return firstElementStyle;
	// }
	//
	// public ObservableValue<String> getElementStyle() {
	// return elementStyle;
	// }

	public ObservableList<COL> getColumns() {
		return columns;
	}

	public Function<COL, ObservableValue<U>> getColumnExtractor() {
		return columnExtractor;
	}
}