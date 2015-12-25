package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.gsadmin.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.TableBuilder.TextCellTableBuilder;

/****************************************************************************************************************/

public abstract class TableModel<ITEM, COL, U, T> {
	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");// TODO set to null do work and disable on firstRowExtractor for solve final pb
	// must be final
	private Function<COL, ObservableValue<U>> firstRowExtractor; // = column -> new ReadOnlyStringWrapper("Column : " + column);// set to null for remove first row
	private Function<ITEM, ObservableValue<String>> rowfirstColumnString = item -> new ReadOnlyStringWrapper("Row : " + item);// set to null for remove first column
	private Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;// = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
	TableStyle tableStyle = new TableStyle();

	public TableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<COL, ObservableValue<U>> firstRowExtractor, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor) {
		this.items = items;
		this.columns = columns;
		this.firstRowExtractor = firstRowExtractor;
		this.rowColumnExtractor = rowColumnExtractor;
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

	public Function<COL, ObservableValue<U>> getFirstRowExtractor() {
		return firstRowExtractor;
	}

	public void setFirstRowExtractor(Function<COL, ObservableValue<U>> firstRowExtractor) {
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
		return getTableBuilder().build(this);
	}

	abstract TableBuilder<ITEM, COL, U, T> getTableBuilder();

	public static class TextTableModel<ITEM, COL> extends TableModel<ITEM, COL, String, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns) {
			super(items, columns, column -> new ReadOnlyStringWrapper("Column : " + column), item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column));
		}

		@Override
		TableBuilder<ITEM, COL, String, String> getTableBuilder() {
			return new TextCellTableBuilder<>();
		}
	}

	public static class TableCellTableModel<ITEM, COL> extends TableModel<ITEM, COL, String, Table> {
		// Function<COL, ObservableValue<Table>> columnExtractor = column -> new ReadOnlyObjectWrapper<Table>(new TextCellTableBuilder<>().build(new TextTableModel<>(FXCollections.observableArrayList(8, 9), FXCollections.observableArrayList(7, 6))));

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns) {
			super(items, columns, column -> new ReadOnlyStringWrapper("Column : " + column), item -> column -> new ReadOnlyObjectWrapper<Table>(new TextCellTableBuilder<>().build(new TextTableModel<>(FXCollections.observableArrayList(8, 9), FXCollections
					.observableArrayList(7, 6)))));
		}

		@Override
		TableBuilder<ITEM, COL, String, Table> getTableBuilder() {
			return new TableCellTableBuilder<>();
		}
	}

}