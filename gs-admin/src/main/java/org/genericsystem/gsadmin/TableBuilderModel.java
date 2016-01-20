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
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilderModel<ITEM, COL, T> {

	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");
	private Function<COL, ObservableValue<String>> firstRowExtractor = column -> new ReadOnlyStringWrapper("Column : " + column);
	private Function<ITEM, ObservableValue<String>> firstColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);
	private Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;
	private final ObservableValue<String> firstRowLastColumnString = new ReadOnlyStringWrapper("Action");
	private Function<ITEM, ObservableValue<String>> lastColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);
	private TableStyle tableStyle = new TableStyle();
	private Function<ITEM, ObservableValue<T>> secondColumnExtractor;

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

	public Table buildTable() {
		return new Table(buildFirstRow(), buildRows(), tableStyle.table);
	}

	protected ObservableValue<Row> buildFirstRow() {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(buildFirstRow(null, firstColumnExtractor != null ? firstRowFirstColumnString : new SimpleStringProperty(), secondColumnExtractor != null ? firstRowFirstColumnString
				: new SimpleStringProperty(), columns, firstRowExtractor, lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty())) : new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> buildRows() {
		return new Transformation<>(items, item -> buildRow(item));
	}

	protected Row buildRow(ITEM item) {
		return buildRow(item, firstColumnExtractor == null ? new SimpleStringProperty() : firstColumnExtractor.apply(item), secondColumnExtractor != null ? secondColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns,
				rowColumnExtractor != null ? col -> rowColumnExtractor.apply(item).apply(col) : null, lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item));
	}

	public Row buildFirstRow(ITEM item, ObservableValue<String> firstColumnString, ObservableValue<String> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> lastColumnString) {
		return new GenericRow((Generic) item, buildFirstCell(firstColumnString, tableStyle.firstRowFirstCell), buildFistRowSecondCell(secondColumnExtractor, tableStyle.firstRowFirstCell), buildFirstRowCells(columns, columnExtractor,
				tableStyle.firstRowCell), buildLastCell(lastColumnString, tableStyle.firstRowLastCell), tableStyle.firstRow);
	}

	public Row buildRow(ITEM item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString) {
		return new GenericRow((Generic) item, buildFirstCell(firstColumnString, tableStyle.firstCell), buildSecondCell(secondColumnExtractor, tableStyle.secondCell), buildCells(columns, columnExtractor, tableStyle.cell), buildLastCell(lastColumnString,
				tableStyle.lastCell), tableStyle.row);
	}

	protected ObservableValue<Cell<?>> buildFirstCell(ObservableValue<String> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, cellStyle) : null);
	}

	protected ObservableValue<Cell<?>> buildFistRowSecondCell(ObservableValue<String> secondColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(secondColumnString.getValue() != null ? new Cell<>(secondColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildFirstRowCells(ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(columnExtractor.apply(column), cellStyle));
	}

	protected ObservableValue<Cell<?>> buildSecondCell(ObservableValue<T> secondColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(secondColumnString.getValue() != null ? new Cell<>(secondColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildCells(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(columnExtractor.apply(column), cellStyle));
	}

	protected ObservableValue<Cell<?>> buildLastCell(ObservableValue<String> lastColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(lastColumnString.getValue() != null ? new Cell<>(lastColumnString, cellStyle) : null);
	}

	ObservableValue<String> getRowFirstCellStyle() {
		return tableStyle.firstCell;
	}

	ObservableValue<String> getRowSecondCellStyle() {
		return tableStyle.secondCell;
	}

	ObservableValue<String> getRowCellsStyle() {
		return tableStyle.cell;
	}

	ObservableValue<String> getRowLastCellsStyle() {
		return tableStyle.lastCell;
	}

	public static class TextTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<String>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
		}

	}

	public static class TableCellTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, Table> {

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<Table>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<Table>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
		}

		@Override
		public Table buildTable() {
			Table result = super.buildTable();
			/* default values */
			result.getColumnWidth().setValue(300);
			result.getRowHeight().setValue(200);
			result.getFirstRowHeight().setValue(50);
			return result;
		}
	}
}