package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.scene.Group;
import javafx.scene.layout.HBox;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.components.GSLabel;
import org.genericsystem.ui.components.GSVBox;
import org.genericsystem.ui.utils.Transformation;

public class TableList {
	private static final ObservableValue<String> TABLE = new ReadOnlyStringWrapper("table");
	private static final ObservableValue<String> FIRSTROW = new ReadOnlyStringWrapper("firstrow");
	private static final ObservableValue<String> ROW = new ReadOnlyStringWrapper("row");
	private static final ObservableValue<String> FIRSTROWCELL = new ReadOnlyStringWrapper("firstrowcell");
	private static final ObservableValue<String> FIRSTCELL = new ReadOnlyStringWrapper("firstcell");
	private static final ObservableValue<String> CELL = new ReadOnlyStringWrapper("cell");
	private final ObservableValue<String> styleClass;
	private final ObservableValue<String> observableString;
	private final ObservableValue<Row> firstRow;
	private final ObservableList<Row> rows;

	public static void init(Element<Group> scene) {
		GSVBox tablePanel = new GSVBox(scene, Group::getChildren).setPrefWidth(800).setPrefHeight(600).setStyleClass(TableList::getStyleClass);
		{
			GSHBox rowPanel = new GSHBox(tablePanel).select(TableList::getFirstRow).include(Row::init).setStyleClass(Row::getStyleClass);
			GSHBox rowPanels = new GSHBox(tablePanel).forEach(TableList::getRows).include(Row::init).setStyleClass(Row::getStyleClass);
		}
	}

	/****************************************************************************************************************/

	public static <ITEM, COLUMN> TableList create(ObservableList<ITEM> items, ObservableList<ITEM> columns) {
		ObservableValue<String> observableString = new ReadOnlyStringWrapper("Table");
		ObservableValue<Row> firstRow = buildFirstRow(observableString, columns, columnIndex -> new ReadOnlyStringWrapper("Column : " + columnIndex), FIRSTROW, FIRSTROWCELL, FIRSTROWCELL);
		ObservableList<Row> rows = buildRows(item -> new ReadOnlyStringWrapper("Row : " + item), items, item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column), columns, ROW, FIRSTCELL, CELL);
		return new TableList(observableString, firstRow, rows);
	}

	public TableList(ObservableValue<String> observableString, ObservableValue<Row> firstRow, ObservableList<Row> rows) {
		this.styleClass = TABLE;
		this.observableString = observableString;
		this.firstRow = firstRow;
		this.rows = rows;
	}

	private static <COLUMN> ObservableValue<Row> buildFirstRow(ObservableValue<String> firstColumnString, ObservableList<COLUMN> columns, Function<COLUMN, ObservableValue<String>> stringConverter, ObservableValue<String> rowStyleClass,
			ObservableValue<String> firstCellStyleClass, ObservableValue<String> cellStyleClass) {
		return new SimpleObjectProperty<>(new Row(firstColumnString, new ReadOnlyObjectWrapper<>(new Cell(new ReadOnlyStringWrapper("Table"), cellStyleClass)), buildRowCells(columns, stringConverter, firstCellStyleClass), rowStyleClass));
	}

	private static <ITEM, COLUMN> ObservableList<Row> buildRows(Function<ITEM, ObservableValue<String>> itemFirstColumnString, ObservableList<ITEM> items, Function<ITEM, Function<COLUMN, ObservableValue<String>>> itemConverter,
			ObservableList<COLUMN> columns, ObservableValue<String> rowStyleClass, ObservableValue<String> firstCellStyleClass, ObservableValue<String> cellStyleClass) {
		ObservableList<Row> rows = new Transformation<Row, ITEM>(items, item -> new Row(itemFirstColumnString.apply(item), new ReadOnlyObjectWrapper<>(new Cell(new ReadOnlyStringWrapper("Row : " + item), cellStyleClass)), buildRowCells(columns,
				itemConverter.apply(item), firstCellStyleClass), rowStyleClass));
		return rows;
	}

	private static <COLUMN> ObservableList<Cell> buildRowCells(ObservableList<COLUMN> columns, Function<COLUMN, ObservableValue<String>> itemConverter, ObservableValue<String> cellStyleClass) {
		return new Transformation<>(columns, column -> new Cell(itemConverter.apply(column), cellStyleClass));
	}

	public static class Row {
		private final ObservableValue<String> styleClass;
		private final ObservableValue<String> observableString;
		private final ObservableValue<Cell> firstCell;
		private final ObservableList<Cell> cells;

		public static void init(Element<HBox> rowPanel) {
			new GSHBox(rowPanel).select(Row::getFirstCell).include(Cell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
			new GSHBox(rowPanel).forEach(Row::getCells).include(Cell::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell::getStyleClass);
		}

		public Row(ObservableValue<String> observableString, ObservableValue<Cell> firstCell, ObservableList<Cell> cells, ObservableValue<String> styleClass) {
			this.observableString = observableString;
			this.firstCell = firstCell;
			this.cells = cells;
			this.styleClass = styleClass;
		}

		public ObservableValue<String> getStyleClass() {
			return styleClass;
		}

		public ObservableValue<String> getObservableString() {
			return observableString;
		}

		public ObservableList<Cell> getCells() {
			return cells;
		}

		public ObservableValue<Cell> getFirstCell() {
			return firstCell;
		}
	}

	public static class Cell {
		private final ObservableValue<String> styleClass;
		private final ObservableValue<String> observableString;

		public static void init(Element<HBox> cellPanels) {
			new GSLabel(cellPanels, Cell::getObservableString).setPrefWidth(200);
		}

		public Cell(ObservableValue<String> observableString, ObservableValue<String> styleClass) {
			this.styleClass = styleClass;
			this.observableString = observableString;
		}

		public ObservableValue<String> getStyleClass() {
			return styleClass;
		}

		public ObservableValue<String> getObservableString() {
			return observableString;
		}
	}

	/****************************************************************************************************************/

	public ObservableValue<String> getObservableString() {
		return observableString;
	}

	public ObservableValue<Row> getFirstRow() {
		return firstRow;
	}

	public ObservableList<Row> getRows() {
		return rows;
	}

	public ObservableValue<String> getStyleClass() {
		return styleClass;
	}

}
