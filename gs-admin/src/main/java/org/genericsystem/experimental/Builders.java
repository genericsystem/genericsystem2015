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
		private final Function<COL, ObservableValue<String>> firstRowExtractor;
		private final Function<ITEM, ObservableValue<?>> firstColumnExtractor;
		private final Function<ITEM, ObservableValue<String>> lastColumnExtractor;
		private final ObservableList<ITEM> items;
		private final ObservableList<COL> columns;

		public TableBuilder(ObservableValue<String> firstRowFirstColumnString, ObservableValue<String> firstRowLastColumnString, ObservableList<ITEM> items, ObservableList<COL> columns,Function<COL, ObservableValue<String>> firstRowExtractor,Function<ITEM, ObservableValue<?>> firstColumnExtractor,Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			this.firstRowFirstColumnString = firstRowFirstColumnString;
			this.firstRowLastColumnString = firstRowLastColumnString;
			this.firstColumnExtractor = firstColumnExtractor;
			this.firstRowExtractor = firstRowExtractor;
			this.lastColumnExtractor = lastColumnExtractor;
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
		
		public boolean isFirstRowPresent(){
			if(firstRowExtractor!=null)
				return true;
			return false;
		}
		
		public boolean isFirstColumnPresent(){
			if(firstColumnExtractor!=null)
				return true;
			return false;
		}
		
		public boolean isLastColumnPresent(){
			if(lastColumnExtractor!=null)
				return true;
			return false;
		}
	}

	// *************************************************************************************
	public static class RowBuilder<ITEM> {
		private TableBuilder<ITEM, ?> tableBuilder;

		public RowBuilder(TableBuilder<ITEM, ?> tableBuilder) {
			this.tableBuilder = tableBuilder;
		}

		public ObservableList<Row> buildRow(ObservableValue<String> styleClass, ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell) {
			return new Transformation<>(tableBuilder.getItems(), item -> new Row(firstCell, cells, lastCell, styleClass));
		}

		public ObservableValue<Row> buildFirstRow(ObservableValue<String> styleClass){
			if(tableBuilder.isFirstRowPresent())
				return null; //créer le row
			return new SimpleObjectProperty<>();
		}

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
		
		public ObservableValue<Cell<?>> buildFirstRowFirstCell(ObservableValue<String> styleClass){
			if(rowBuilder.getTableBuilder().isFirstColumnPresent() || rowBuilder.getTableBuilder().isFirstRowPresent())
				return null; //créer la cell
			return new SimpleObjectProperty<>();
		}
		
		public ObservableValue<Cell<?>> buildFirstCell(ObservableValue<String> styleClass){
			if(rowBuilder.getTableBuilder().isFirstColumnPresent())
				return null; //créer la cell
			return new SimpleObjectProperty<>();
		}
		
		public ObservableValue<Cell<?>> buildFirstRowLastCell(ObservableValue<String> styleClass){
			if(rowBuilder.getTableBuilder().isFirstRowPresent())
				return null; //créer la cell
			return new SimpleObjectProperty<>();
		}
		
		public ObservableValue<Cell<?>> buildLastCell(ObservableValue<String> styleClass){
			if(rowBuilder.getTableBuilder().isLastColumnPresent())
				return null; //créer la cell
			return new SimpleObjectProperty<>();
		}
		
	}

}
