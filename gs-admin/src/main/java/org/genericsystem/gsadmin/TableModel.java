package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.TableStyle;

/****************************************************************************************************************/

public abstract class TableModel<ITEM, COL, T> {
	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");
	Function<COL, ObservableValue<String>> firstRowExtractor = column -> new ReadOnlyStringWrapper("Column : " + column);// set to null for remove first row
	Function<ITEM, ObservableValue<String>> rowfirstColumnString = item -> new ReadOnlyStringWrapper("Row : " + item);// set to null for remove first column
	Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;// = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
	TableStyle tableStyle = new TableStyle();

	public TableModel(ObservableList<ITEM> items, ObservableList<COL> columns) {
		this.items = items;
		this.columns = columns;
	}

	public void disableFirstRow() {
		firstRowExtractor = null;
	}

	public void disableFirstColumn() {
		rowfirstColumnString = null;
	}

	public ObservableList<ITEM> getItems() {
		return items;
	}

	public ObservableList<COL> getColumns() {
		return columns;
	}

	public ObservableValue<String> getFirstRowFirstColumnString() {
		return firstRowFirstColumnString;
	}

	public void setFirstRowFirstColumnString(ObservableValue<String> firstRowFirstColumnString) {
		this.firstRowFirstColumnString = firstRowFirstColumnString;
	}

	public Function<COL, ObservableValue<String>> getFirstRowExtractor() {
		return firstRowExtractor;
	}

	public void setFirstRowExtractor(Function<COL, ObservableValue<String>> firstRowExtractor) {
		this.firstRowExtractor = firstRowExtractor;
	}

	public Function<ITEM, ObservableValue<String>> getRowfirstColumnString() {
		return rowfirstColumnString;
	}

	public void setRowfirstColumnString(Function<ITEM, ObservableValue<String>> rowfirstColumnString) {
		this.rowfirstColumnString = rowfirstColumnString;
	}

	public Function<ITEM, Function<COL, ObservableValue<T>>> getRowColumnExtractor() {
		return rowColumnExtractor;
	}

	public void setRowColumnExtractor(Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor) {
		this.rowColumnExtractor = rowColumnExtractor;
	}

	public TableStyle getTableStyle() {
		return tableStyle;
	}

	public void setTableStyle(TableStyle tableStyle) {
		this.tableStyle = tableStyle;
	}

	public Table createTable() {
		return new TableBuilder<ITEM, COL>() {
		}.build(this);
	}

	public static class TextTableModel<ITEM, COL> extends TableModel<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns) {
			super(items, columns);
			rowColumnExtractor = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
		}
	}

	public static class TableCellTableModel<ITEM, COL> extends TableModel<ITEM, COL, Table> {

		Function<COL, ObservableValue<Table>> columnExtractor = column -> new ReadOnlyObjectWrapper<Table>(new TableBuilder() {
		}.build(new TextTableModel<>(FXCollections.observableArrayList(8, 9), FXCollections.observableArrayList(7, 6))));

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns) {
			super(items, columns);
			rowColumnExtractor = item -> columnExtractor;
		}
	}

}