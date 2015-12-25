package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.TableStyle;

class RowModel<COL, U, T> extends Model {
	final ObservableValue<String> firstColumnString;
	final Function<COL, ObservableValue<U>> columnExtractor;
	final TableStyle tableStyle;
	final ObservableList<COL> columns;

	RowModel(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<U>> columnExtractor, TableStyle tableStyle/* , ObservableValue<String> firstElementStyle, ObservableValue<String> elementStyle */) {
		this.firstColumnString = firstColumnString;
		this.columnExtractor = columnExtractor;
		this.tableStyle = tableStyle;
		this.columns = columns;
	}

	public ObservableValue<String> getFirstColumnString() {
		return firstColumnString;
	}

	@Override
	public TableStyle getTableStyle() {
		return tableStyle;
	}

	public ObservableList<COL> getColumns() {
		return columns;
	}

	public Function<COL, ObservableValue<U>> getColumnExtractor() {
		return columnExtractor;
	}
}