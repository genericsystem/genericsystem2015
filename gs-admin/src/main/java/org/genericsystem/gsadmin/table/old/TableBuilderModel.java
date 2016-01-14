package org.genericsystem.gsadmin.table.old;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.table.old.Stylable.TableStyle;
import org.genericsystem.gsadmin.table.old.TableBuilder.TableCellTableBuilder;
import org.genericsystem.gsadmin.table.old.TableBuilder.TextCellTableBuilder;

public abstract class TableBuilderModel<ITEM, COL, T> {
	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");// TODO set to null do work and disable on firstRowExtractor for solve final pb
	private Function<COL, ObservableValue<String>> firstRowExtractor = column -> new ReadOnlyStringWrapper("Column : " + column);// set to null for remove first row
	private Function<ITEM, ObservableValue<String>> firstColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);// set to null for remove first column
	private Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;// = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
	private TableStyle tableStyle = new TableStyle();

	public TableBuilderModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
			Function<ITEM, ObservableValue<String>> firstColumnExtractor) {
		this.items = items;
		this.columns = columns;
		this.firstColumnExtractor = firstColumnExtractor;
		this.rowColumnExtractor = rowColumnExtractor;
		this.firstRowExtractor = firstRowExtractor;
	}

	public void disableFirstRow() {
		firstRowExtractor = null;
	}

	public void disableFirstColumn() {
		firstColumnExtractor = null;
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

	public Function<COL, ObservableValue<String>> getFirstRowExtractor() {
		return firstRowExtractor;
	}

	public void setFirstRowExtractor(Function<COL, ObservableValue<String>> firstRowExtractor) {
		this.firstRowExtractor = firstRowExtractor;
	}

	public Function<ITEM, ObservableValue<String>> getFirstColumnExtractor() {
		return firstColumnExtractor;
	}

	public void setRowfirstColumnString(Function<ITEM, ObservableValue<String>> rowfirstColumnString) {
		this.firstColumnExtractor = rowfirstColumnString;
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
		return getTableBuilder().build(items, firstRowFirstColumnString, columns, firstRowExtractor, firstColumnExtractor, rowColumnExtractor, tableStyle);
	}

	abstract TableBuilder<ITEM, COL, T> getTableBuilder();

	public static class TextTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor) {
			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor);
		}

		@Override
		TableBuilder<ITEM, COL, String> getTableBuilder() {
			return new TextCellTableBuilder<>();
		}
	}

	public static class TableCellTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, Table> {

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor) {

			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor);
		}

		@Override
		TableBuilder<ITEM, COL, Table> getTableBuilder() {
			return new TableCellTableBuilder<>();
		}

		@Override
		public Table createTable() {
			Table result = super.createTable();
			/* default values */
			result.getColumnWidth().setValue(300);
			result.getRowHeight().setValue(200);
			result.getFirstRowHeight().setValue(50);
			return result;
		}
	}
}