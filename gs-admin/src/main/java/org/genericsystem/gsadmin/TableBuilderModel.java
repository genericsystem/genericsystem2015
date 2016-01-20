package org.genericsystem.gsadmin;

import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellFirstRowBuilder;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilderModel<ITEM, COL, T> {

	protected final ObservableList<ITEM> items;
	protected final ObservableList<COL> columns;
	protected final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");// TODO set to null do work and disable on firstRowExtractor for solve final pb
	protected Function<COL, ObservableValue<String>> firstRowExtractor = column -> new ReadOnlyStringWrapper("Column : " + column);// set to null for remove first row
	protected Function<ITEM, ObservableValue<String>> firstColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);// set to null for remove first column
	protected Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;// = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
	protected final ObservableValue<String> firstRowLastColumnString = new ReadOnlyStringWrapper("Action");
	// private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Test");
	protected Function<ITEM, ObservableValue<String>> lastColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);
	protected TableStyle tableStyle = new TableStyle();
	protected Function<ITEM, ObservableValue<T>> secondColumnExtractor;

	public TableBuilderModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
			Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<T>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
		this.items = items;
		this.columns = columns;
		this.firstColumnExtractor = firstColumnExtractor;
		this.rowColumnExtractor = rowColumnExtractor;
		this.firstRowExtractor = firstRowExtractor;
		this.secondColumnExtractor = secondColumnExtractor;
		this.lastColumnExtractor = lastColumnExtractor;
	}

	public void disableFirstRow() {
		firstRowExtractor = null;
	}

	public Function<ITEM, ObservableValue<T>> getSecondColumnExtractor() {
		return secondColumnExtractor;
	}

	public void disableFirstColumn() {
		firstColumnExtractor = null;
	}

	public void disableLastRow() {
		firstRowExtractor = null;
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

	public ObservableValue<String> getFirstRowLastColumnString() {
		return firstRowLastColumnString;
	}

	public ObservableValue<String> getFirstRowSecondColumnString() {
		return firstRowFirstColumnString;
	}

	public Function<ITEM, ObservableValue<String>> getLastColumnExtractor() {
		return lastColumnExtractor;
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
		return build(items, firstRowFirstColumnString, firstRowFirstColumnString, columns, firstRowExtractor, firstRowLastColumnString, firstColumnExtractor, secondColumnExtractor, rowColumnExtractor, lastColumnExtractor, tableStyle);
	}

	public Table build(ObservableList<ITEM> items, ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowSecondColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> firstRowExtractor,
			ObservableValue<String> firstRowLastColumnString, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<T>> secondColumnExtractor, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor,
			Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {

		return new Table(getFirstElement(secondColumnExtractor, firstRowFirstColumnString, firstRowSecondColumnString, columns, firstRowExtractor, firstRowLastColumnString, firstColumnExtractor, lastColumnExtractor, tableStyle), getElements(items,
				firstColumnExtractor, secondColumnExtractor, columns, rowColumnExtractor, lastColumnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.table;
	}

	protected ObservableValue<Row> getFirstElement(Function<ITEM, ObservableValue<T>> secondColumnExtractor, ObservableValue<String> firstColumnString, ObservableValue<String> firstRowSecondColumnString, ObservableList<COL> columns,
			Function<COL, ObservableValue<String>> firstRowExtractor, ObservableValue<String> firstRowLastColumnString, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor,
			TableStyle tableStyle) {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(new TextCellFirstRowBuilder<COL>().build(null, firstColumnExtractor != null ? firstColumnString : new SimpleStringProperty(), secondColumnExtractor != null ? firstRowSecondColumnString
				: new SimpleStringProperty(), columns, firstRowExtractor, lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty(), tableStyle)) : new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> getElements(ObservableList<ITEM> items, Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<T>> secondColumnExtractor, ObservableList<COL> columns,
			Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor, TableStyle tableStyle) {

		return new Transformation<Row, ITEM>(items, item -> buildRow(item));
	}

	protected Row buildRow(ITEM item) {
		return buildRow(item, firstColumnExtractor == null ? new SimpleStringProperty() : firstColumnExtractor.apply(item), secondColumnExtractor != null ? secondColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns,
				rowColumnExtractor != null ? col -> rowColumnExtractor.apply(item).apply(col) : null, lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item), tableStyle);
	}

	public Row buildRow(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
			TableStyle tableStyle) {
		return new GenericRow((Generic) item, getFirstCell(firstColumnString, tableStyle), getSecondCell(secondColumnExtractor, tableStyle), getCells(columns, columnExtractor, tableStyle), getLastCell(lastColumnString, tableStyle), getRowStyle(tableStyle));
	}

	protected ObservableValue<String> getRowStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

	protected ObservableValue<Cell<?>> getFirstCell(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, getRowFirstCellStyle(tableStyle)) : null);
	}

	protected ObservableValue<Cell<?>> getSecondCell(ObservableValue<T> secondColumnString, TableStyle tableStyle) {
		return new ReadOnlyObjectWrapper<>(secondColumnString.getValue() != null ? new Cell<>(secondColumnString, getRowSecondCellStyle(tableStyle)) : null);
	}

	abstract ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle);

	abstract ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle);

	abstract ObservableValue<String> getRowCellsStyle(TableStyle tableStyle);

	abstract ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle);

	protected ObservableList<Cell<?>> getCells(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(columnExtractor.apply(column), getRowCellsStyle(tableStyle)));
	}

	protected ObservableValue<Cell<?>> getLastCell(ObservableValue<String> lastColumnString, TableStyle tableStyle) {
		return new ReadOnlyObjectWrapper<>(lastColumnString.getValue() != null ? new Cell<>(lastColumnString, getRowLastCellsStyle(tableStyle)) : null);
	}

	public static class TextTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
		}

		@Override
		ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle) {
			return tableStyle.secondCell;
		}

		@Override
		ObservableValue<String> getRowCellsStyle(TableStyle tableStyle) {
			return tableStyle.cell;
		}

		@Override
		ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle) {
			return tableStyle.lastCell;
		}

	}

	public static class TableCellTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, Table> {

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<Table>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {

			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
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

		@Override
		ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle) {
			return tableStyle.secondCell;
		}

		@Override
		ObservableValue<String> getRowCellsStyle(TableStyle tableStyle) {
			return tableStyle.cell;
		}

		@Override
		ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle) {
			return tableStyle.lastCell;
		}
	}
}