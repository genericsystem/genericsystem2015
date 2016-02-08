package org.genericsystem.experimental;

import java.util.function.Function;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public interface Builders {

	public static class TableBuilder<ITEM, COL> {

		private final ObservableValue<String> firstRowFirstColumnString;
		private final ObservableValue<String> firstRowLastColumnString;
		private final ObservableList<ITEM> items;
		private final ObservableList<COL> columns;

		public TableBuilder(ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowLastColumnString, ObservableList<ITEM> items, ObservableList<COL> columns) {
			this.firstRowFirstColumnString = firstRowFirstColumnString;
			this.firstRowLastColumnString = firstRowLastColumnString;
			this.items = items;
			this.columns = columns;
		}

		public ObservableValue<Table> buildTable(int width, int height, ObservableValue<String> tableStyle, ObservableValue<Row> firstRow, ObservableList<Row> rows) {
			return new SimpleObjectProperty<>(new Table(width, height, firstRow, rows, tableStyle));
		}

		public ObservableValue<String> getFirstRowFirstColumnString() {
			return firstRowFirstColumnString;
		}

		public ObservableValue<String> getFirstRowLastColumnString() {
			return firstRowLastColumnString;
		}

		public ObservableList<ITEM> getItems() {
			return items;
		}

		public ObservableList<COL> getColumns() {
			return columns;
		}
	}

	// *************************************************************************************
	public static class RowBuilder<ITEM> {
		// private ObservableValue<Cell<?>> firstCell;
		// private ObservableList<Cell<?>> cells;
		// private ObservableValue<Cell<?>> lastCell;
		private TableBuilder<ITEM, ?> tableBuilder;

		public RowBuilder(TableBuilder<ITEM, ?> tableBuilder) {
			this.tableBuilder = tableBuilder;
		}

		public ObservableList<Row> buildRow(ObservableValue<String> styleClass, ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell) {
			return new Transformation<>(tableBuilder.getItems(), item -> new Row(firstCell, cells, lastCell, styleClass));
		}

		// public ObservableValue<Cell<?>> getFirstCell() {
		// return firstCell;
		// }
		//
		// public void setFirstCell(ObservableValue<Cell<?>> firstCell) {
		// this.firstCell = firstCell;
		// }
		//
		// public ObservableList<Cell<?>> getCells() {
		// return cells;
		// }
		//
		// public void setCells(ObservableList<Cell<?>> cells) {
		// this.cells = cells;
		// }
		//
		// public ObservableValue<Cell<?>> getLastCell() {
		// return lastCell;
		// }
		//
		// public void setLastCell(ObservableValue<Cell<?>> lastCell) {
		// this.lastCell = lastCell;
		// }

		public TableBuilder<ITEM, ?> getTableBuilder() {
			return tableBuilder;
		}

		public void setTableBuilder(TableBuilder<ITEM, ?> tableBuilder) {
			this.tableBuilder = tableBuilder;
		}

	}

	// ***********************************************************************************

	public static class CellBuilder<ITEM, COL, T> {

		private final RowBuilder<?> rowBuilder;
		private final Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;

		public CellBuilder(RowBuilder<ITEM> rowBuilder, Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor) {
			this.rowBuilder = rowBuilder;
			this.rowColumnExtractor = rowColumnExtractor;
		}

		public ObservableList<Cell<T>> buildCell(ObservableValue<String> styleClass) {
			ObservableList<Cell<T>> listCell = FXCollections.emptyObservableList();

			rowBuilder.getTableBuilder().getItems().forEach(item -> {
				rowBuilder.getTableBuilder().getColumns().forEach(col -> {

					listCell.add(new Cell<T>(rowColumnExtractor.apply((ITEM) item).apply((COL) col), styleClass));
				});
			});

			return listCell;
		}

		// public <IITEM, ICOL> ObservableList<Cell<Table>> buildTextCellTable(ObservableValue<String> styleClass, Function<ITEM, ObservableList<IITEM>> itemExtractor, Function<COL, ObservableList<ICOL>> columnExtractor,
		// Function<IITEM, Function<ICOL, ObservableValue<String>>> rowColumnExtractor) {
		// ObservableList<Cell<Table>> listCell = FXCollections.emptyObservableList();
		//
		// rowBuilder.getTableBuilder().getItems().forEach(item -> {
		// rowBuilder.getTableBuilder().getColumns().forEach(col -> {
		//
		// TableBuilder<IITEM, ICOL> tableBuilder = new TableBuilder<IITEM, ICOL>(null, null, itemExtractor.apply((ITEM) item), columnExtractor.apply((COL) col));
		// RowBuilder<IITEM> rowBuilder = new RowBuilder<IITEM>(tableBuilder);
		// CellBuilder<IITEM, ICOL, String> cellBuilder = new CellBuilder<>(rowBuilder, rowColumnExtractor);
		//
		// cellBuilder.buildTextCell(styleClass);
		// rowBuilder.buildRow(styleClass);
		// ObservableValue<Table> table = tableBuilder.buildTable(500, 500, styleClass);
		// listCell.add(new Cell<Table>(table, styleClass));
		//
		// });
		// });
		// return listCell;
		// }
	}

}
