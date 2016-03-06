package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.distributed.ui.Transformation;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.Table.FirstColumnTable;

public abstract class TableBuilder<ITEM, COL, T> {

	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString;
	private Function<COL, ObservableValue<String>> firstRowExtractor;
	private final Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;
	private final ObservableValue<String> firstRowLastColumnString;
	private final Function<ITEM, ObservableValue<String>> lastColumnExtractor;
	private TableStyle tableStyle = new TableStyle();
	private final Function<ITEM, ObservableValue<T>> firstColumnExtractor;

	public TableBuilder(ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowLastColumnString, ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor,
			Function<COL, ObservableValue<String>> firstRowExtractor, Function<ITEM, ObservableValue<T>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
		this.firstRowFirstColumnString = firstRowFirstColumnString;
		this.firstRowLastColumnString = firstRowLastColumnString;
		this.items = items;
		this.columns = columns;
		this.firstColumnExtractor = firstColumnExtractor;
		this.rowColumnExtractor = rowColumnExtractor;
		this.firstRowExtractor = firstRowExtractor;
		this.lastColumnExtractor = lastColumnExtractor;
	}

	public Table buildTable(int width, int height) {
		return new Table(width, height, buildFirstRow(), buildRows(), tableStyle.table);
	}

	public Table buildTableFirstColumn() {
		return new FirstColumnTable(buildFirstRow(), buildRows(), tableStyle.table);
	}

	protected ObservableValue<Row> buildFirstRow() {
		return firstRowExtractor != null ? new SimpleObjectProperty<>(buildFirstRow(null, firstColumnExtractor != null ? firstRowFirstColumnString : new SimpleStringProperty(), columns, firstRowExtractor,
				lastColumnExtractor != null ? firstRowLastColumnString : new SimpleStringProperty())) : new SimpleObjectProperty<>();
	}

	protected ObservableList<Row> buildRows() {
		return new Transformation<>(items, item -> buildRow(item));
	}

	protected Row buildRow(ITEM item) {
		return buildRow(item, firstColumnExtractor != null ? firstColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns, rowColumnExtractor != null ? col -> rowColumnExtractor.apply(item).apply(col) : null,
				lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item));
	}

	public Row buildFirstRow(ITEM item, ObservableValue<String> firstColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> lastColumnString) {
		return new GenericRow((Generic) item, buildFirstRowFirstCell(firstColumnExtractor, tableStyle.firstRowFirstCell), buildFirstRowCells(columns, columnExtractor, tableStyle.firstRowCell), buildLastCell(lastColumnString, tableStyle.firstRowLastCell),
				tableStyle.firstRow);
	}

	public Row buildRow(ITEM item, ObservableValue<T> firstColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString) {
		return new GenericRow((Generic) item, buildFirstCell(firstColumnExtractor, tableStyle.firstCell), buildCells(columns, columnExtractor, tableStyle.cell), buildLastCell(lastColumnString, tableStyle.lastCell), tableStyle.row);
	}

	protected ObservableValue<Cell<?>> buildFirstRowFirstCell(ObservableValue<String> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, cellStyle) : null);
	}

	protected ObservableValue<Cell<?>> buildFirstCell(ObservableValue<T> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, cellStyle) : null);
	}

	protected ObservableValue<Cell<?>> buildFistRowfirstCell(ObservableValue<String> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildFirstRowCells(ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> new Cell<>(columnExtractor.apply(column), cellStyle));
	}

	protected ObservableValue<Cell<?>> buildfirstCell(ObservableValue<T> firstColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(firstColumnString.getValue() != null ? new Cell<>(firstColumnString, cellStyle) : null);
	}

	protected ObservableList<Cell<?>> buildCells(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> cellStyle) {
		return columnExtractor == null ? FXCollections.emptyObservableList() : new Transformation<>(columns, column -> {
			return new Cell<>(columnExtractor.apply(column), cellStyle);
		});
	}

	protected ObservableValue<Cell<?>> buildLastCell(ObservableValue<String> lastColumnString, ObservableValue<String> cellStyle) {
		return new ReadOnlyObjectWrapper<>(lastColumnString.getValue() != null ? new Cell<>(lastColumnString, cellStyle) : null);
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

	public static class TextTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, String> {
		public TextTableBuilder(ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowLastColumnString, ObservableList<ITEM> items, ObservableList<COL> columns,
				Function<ITEM, Function<COL, ObservableValue<String>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor, Function<ITEM, ObservableValue<String>> firstColumnExtractor,
				Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(firstRowFirstColumnString, firstRowLastColumnString, items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, lastColumnExtractor);
		}
	}

	public static class TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL, Table> {
		public TableCellTableBuilder(ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowLastColumnString, ObservableList<ITEM> items, ObservableList<COL> columns,
				Function<ITEM, Function<COL, ObservableValue<Table>>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor, Function<ITEM, ObservableValue<Table>> firstColumnExtractor,
				Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(firstRowFirstColumnString, firstRowLastColumnString, items, columns, rowColumnExtractor, firstRowExtractor, firstColumnExtractor, lastColumnExtractor);
		}
	}
}