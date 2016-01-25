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
import org.genericsystem.ui.Model;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilder<ITEM, COL, T> {

	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");
	private Function<COL, ObservableValue<String>> firstRowExtractor;
	// private Function<ITEM, ObservableValue<String>> firstColumnExtractor;
	private final Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;
	private final ObservableValue<String> firstRowLastColumnString = new ReadOnlyStringWrapper("Action");
	private final Function<ITEM, ObservableValue<String>> lastColumnExtractor;
	private TableStyle tableStyle = new TableStyle();
	private final Function<ITEM, ObservableValue<T>> firstColumnExtractor;

	public TableBuilder(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
	/* Function<ITEM, ObservableValue<String>> firstColumnExtractor, */Function<ITEM, ObservableValue<T>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
		this.items = items;
		this.columns = columns;
		// this.firstColumnExtractor = firstColumnExtractor;
		this.firstColumnExtractor = firstColumnExtractor;
		this.rowColumnExtractor = rowColumnExtractor;
		this.firstRowExtractor = firstRowExtractor;
		this.lastColumnExtractor = lastColumnExtractor;
	}

	public void disableFirstRow() {
		firstRowExtractor = null;
	}

	public Function<ITEM, ObservableValue<T>> getfirstColumnExtractor() {
		return firstColumnExtractor;
	}

	// public void disableFirstColumn() {
	// firstColumnExtractor = null;
	// }

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

	// public Function<ITEM, ObservableValue<String>> getFirstColumnExtractor() {
	// return firstColumnExtractor;
	// }
	//
	// public void setRowfirstColumnString(Function<ITEM, ObservableValue<String>> rowfirstColumnString) {
	// this.firstColumnExtractor = rowfirstColumnString;
	// }

	public Function<ITEM, Function<COL, ObservableValue<T>>> getRowColumnExtractor() {
		return rowColumnExtractor;
	}

	public ObservableValue<String> getFirstRowLastColumnString() {
		return firstRowLastColumnString;
	}

	public ObservableValue<String> getFirstRowfirstColumnString() {
		return firstRowFirstColumnString;
	}

	public Function<ITEM, ObservableValue<String>> getLastColumnExtractor() {
		return lastColumnExtractor;
	}

	public TableStyle getTableStyle() {
		return tableStyle;
	}

	public void setTableStyle(TableStyle tableStyle) {
		this.tableStyle = tableStyle;
	}

	public Table buildTable(Model parent) {
		Table table = new Table(parent);
		table.setFirstElement(buildFirstRow(table));
		table.setElements(buildRows(table));
		return table;
		// return new Table(parent, buildFirstRow(), buildRows(), tableStyle.table);
	}

	protected ObservableValue<Row> buildFirstRow(Table parent) {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(buildFirstRow(parent, null, firstColumnExtractor != null ? firstRowFirstColumnString : new SimpleStringProperty(), columns, firstRowExtractor,
				lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty())) : new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> buildRows(Table parent) {
		return new Transformation<>(items, item -> buildRow(parent, item));
	}

	protected Row buildRow(Table parent, ITEM item) {
		return buildRow(parent, item, firstColumnExtractor != null ? firstColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns, rowColumnExtractor != null ? col -> rowColumnExtractor.apply(item).apply(col) : null,
				lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item));
	}

	public Row buildFirstRow(Table parent, ITEM item, ObservableValue<String> firstColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> lastColumnString) {
		GenericRow firstRow = new GenericRow(parent);
		firstRow.setItem((Generic) item);
		firstRow.setFirstElement(buildFirstCell(firstRow, (ObservableValue<T>) firstColumnExtractor, tableStyle.firstRowFirstCell));
		firstRow.setElements(buildFirstRowCells(firstRow, columns, columnExtractor, tableStyle.firstRowCell));
		firstRow.setLastElement(buildLastCell(firstRow, lastColumnString, tableStyle.firstRowLastCell));

		return firstRow;
		// return new GenericRow(parent, (Generic) item, buildFirstCell((ObservableValue<T>) firstColumnExtractor, tableStyle.firstRowFirstCell), buildFirstRowCells(columns, columnExtractor, tableStyle.firstRowCell), buildLastCell(lastColumnString,
		// tableStyle.firstRowLastCell), tableStyle.firstRow);
	}

	public Row buildRow(Table parent, ITEM item, ObservableValue<T> firstColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString) {
		GenericRow currentRow = new GenericRow(parent);
		currentRow.setItem((Generic) item);
		currentRow.setFirstElement(buildFirstCell(currentRow, firstColumnExtractor, tableStyle.firstCell));
		currentRow.setElements(buildCells(currentRow, columns, columnExtractor, tableStyle.cell));
		currentRow.setLastElement(buildLastCell(currentRow, lastColumnString, tableStyle.lastCell));
		return currentRow;
		// return new GenericRow(parent, (Generic) item, buildFirstCell(firstColumnExtractor, tableStyle.firstCell), buildCells(columns, columnExtractor, tableStyle.cell), buildLastCell(lastColumnString, tableStyle.lastCell), tableStyle.row);
	}

	protected ObservableValue<Cell<?>> buildFirstCell(Row parent, ObservableValue<T> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(parent, firstColumnString, cellStyle) : null);
	}

	protected ObservableValue<Cell<?>> buildFistRowfirstCell(Row parent, ObservableValue<String> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(parent, firstColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildFirstRowCells(Row parent, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(parent, columnExtractor.apply(column), cellStyle));
	}

	protected ObservableValue<Cell<?>> buildfirstCell(Row parent, ObservableValue<T> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(parent, firstColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildCells(Row parent, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(parent, columnExtractor.apply(column), cellStyle));
	}

	protected ObservableValue<Cell<?>> buildLastCell(Row parent, ObservableValue<String> lastColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(lastColumnString.getValue() != null ? new Cell<>(parent, lastColumnString, cellStyle) : null);
	}

	ObservableValue<String> getRowFirstCellStyle() {
		return tableStyle.firstCell;
	}

	ObservableValue<String> getRowfirstCellStyle() {
		return tableStyle.firstCell;
	}

	ObservableValue<String> getRowCellsStyle() {
		return tableStyle.cell;
	}

	ObservableValue<String> getRowLastCellsStyle() {
		return tableStyle.lastCell;
	}

	public static class TextTableModel<ITEM, COL> extends TableBuilder<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<String>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, lastColumnExtractor);
		}

	}

	public static class TableCellTableModel<ITEM, COL> extends TableBuilder<ITEM, COL, Table> {

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<Table>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<Table>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, lastColumnExtractor);
		}

		@Override
		public Table buildTable(Model parent) {
			Table result = super.buildTable(parent);
			/* default values */
			result.getColumnWidth().setValue(300);
			result.getRowHeight().setValue(200);
			result.getFirstRowHeight().setValue(50);
			return result;
		}
	}
}