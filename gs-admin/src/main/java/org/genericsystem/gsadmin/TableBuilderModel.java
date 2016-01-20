package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyStringWrapper;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.GenericRowBuilders.TableCellRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellFirstRowBuilder;
import org.genericsystem.gsadmin.GenericRowBuilders.TextCellRowBuilder;
import org.genericsystem.gsadmin.GenericTableBuilders.TableCellTableBuilder;
import org.genericsystem.gsadmin.GenericTableBuilders.TextCellTableBuilder;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.TableBuilder;
import org.genericsystem.ui.utils.Transformation;

public abstract class TableBuilderModel<ITEM, COL, T> {

	private final ObservableList<ITEM> items;
	private final ObservableList<COL> columns;
	private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Table");// TODO set to null do work and disable on firstRowExtractor for solve final pb
	private Function<COL, ObservableValue<String>> firstRowExtractor = column -> new ReadOnlyStringWrapper("Column : " + column);// set to null for remove first row
	private Function<ITEM, ObservableValue<String>> firstColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);// set to null for remove first column
	private Function<ITEM, Function<COL, ObservableValue<T>>> rowColumnExtractor;// = item -> column -> new ReadOnlyStringWrapper("Cell : " + item + " " + column);
	private final ObservableValue<String> firstRowLastColumnString = new ReadOnlyStringWrapper("Action");
	// private final ObservableValue<String> firstRowFirstColumnString = new ReadOnlyStringWrapper("Test");
	private Function<ITEM, ObservableValue<String>> lastColumnExtractor = item -> new ReadOnlyStringWrapper("Row : " + item);
	private TableStyle tableStyle = new TableStyle();
	Function<ITEM, ObservableValue<T>> secondColumnExtractor;

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

		return new Transformation<Row, ITEM>(items, item -> getRowBuilder().build(item, firstColumnExtractor == null ? new SimpleStringProperty() : firstColumnExtractor.apply(item),
				secondColumnExtractor != null ? secondColumnExtractor.apply(item) : new SimpleObjectProperty<T>(), columns, rowColumnExtractor != null ? col -> rowColumnExtractor.apply(item).apply(col) : null,
				lastColumnExtractor == null ? new SimpleStringProperty() : lastColumnExtractor.apply(item), tableStyle));
	}

	// public Row buildRow(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
	// TableStyle tableStyle) {
	// return new GenericRow((Generic) item, getFirstElement(firstColumnString, tableStyle), getSecondElement(secondColumnExtractor, tableStyle), getElements(columns, columnExtractor, tableStyle), getLastElement(lastColumnString, tableStyle),
	// getStyle(tableStyle));
	// }

	// protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
	// if (firstColumnString.getValue() == null)
	// return new ReadOnlyObjectWrapper<>();
	// return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	// }
	//
	// protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
	// if (columnExtractor == null)
	// return FXCollections.emptyObservableList();
	// return new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), tableStyle));
	// }
	//
	// protected ObservableValue<Cell<?>> getLastElement(ObservableValue<String> lastColumnString, TableStyle tableStyle) {
	// if (lastColumnString.getValue() == null)
	// return new ReadOnlyObjectWrapper<>();
	// return new ReadOnlyObjectWrapper<>(getRowLastCellBuilder().build(lastColumnString, tableStyle));
	// }
	//
	// protected ObservableValue<Cell<?>> getSecondElement(ObservableValue<T> secondColumnString, TableStyle tableStyle) {
	// if (secondColumnString.getValue() == null)
	// return new ReadOnlyObjectWrapper<>();
	// return new ReadOnlyObjectWrapper<>(getSecondCellBuilder().build(secondColumnString, tableStyle));
	// }

	public abstract RowBuilder<COL, T> getRowBuilder();

	abstract TableBuilder<ITEM, COL, T> getTableBuilder();

	public static class TextTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, String> {
		public TextTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<String>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {
			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
		}

		@Override
		TableBuilder<ITEM, COL, String> getTableBuilder() {
			return (TableBuilder<ITEM, COL, String>) new TextCellTableBuilder();
		}

		@Override
		public RowBuilder<COL, String> getRowBuilder() {
			return new TextCellRowBuilder<COL>();
		}
	}

	public static class TableCellTableModel<ITEM, COL> extends TableBuilderModel<ITEM, COL, Table> {

		public TableCellTableModel(ObservableList<ITEM> items, ObservableList<COL> columns, Function<ITEM, Function<COL, ObservableValue>> rowColumnExtractor, Function<COL, ObservableValue<String>> firstRowExtractor,
				Function<ITEM, ObservableValue<String>> firstColumnExtractor, Function<ITEM, ObservableValue<Table>> secondColumnExtractor, Function<ITEM, ObservableValue<String>> lastColumnExtractor) {

			super(items, columns, (Function) rowColumnExtractor, firstRowExtractor, firstColumnExtractor, secondColumnExtractor, lastColumnExtractor);
		}

		@Override
		TableBuilder<ITEM, COL, Table> getTableBuilder() {
			return (TableBuilder<ITEM, COL, Table>) new TableCellTableBuilder();
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
		public RowBuilder<COL, Table> getRowBuilder() {
			return new TableCellRowBuilder<COL>();
		}
	}
}