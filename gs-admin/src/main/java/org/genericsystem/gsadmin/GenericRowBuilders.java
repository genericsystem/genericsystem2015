package org.genericsystem.gsadmin;

import java.util.function.Function;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.utils.Transformation;

public abstract class GenericRowBuilders<COL, T> extends RowBuilder<COL, T> {

	@Override
	public Row build(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
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

	public static class TextCellRowBuilder<COL> extends GenericRowBuilders<COL, String> {

		@Override
		ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowCellsStyle(TableStyle tableStyle) {
			return tableStyle.cell;
		}

		@Override
		ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle) {
			return tableStyle.lastCell;
		}

		// @Override
		// protected CellBuilder<String> getRowFirstCellBuilder() {
		// return new RowFirstCellTextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getCellBuilder() {
		// return new TextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getRowLastCellBuilder() {
		// return new RowLastCellButtonCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getSecondCellBuilder() {
		// return new SecondCellBuilder();
		// }

	}

	public static class TextCellFirstRowBuilder<COL> extends TextCellRowBuilder<COL> {

		@Override
		ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		ObservableValue<String> getRowCellsStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}

		@Override
		ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle) {
			return tableStyle.firstRowLastCell;
		}

		// @Override
		// protected CellBuilder<String> getRowFirstCellBuilder() {
		// return new FirstRowFirstCellTextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getCellBuilder() {
		// return new FirstRowTextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getRowLastCellBuilder() {
		// return new FirstRowLastCellTextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<String> getSecondCellBuilder() {
		// return new SecondCellBuilder();
		// }
	}

	public static final class TableCellRowBuilder<COL> extends GenericRowBuilders<COL, Table> {
		@Override
		ObservableValue<String> getRowFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowSecondCellStyle(TableStyle tableStyle) {
			return tableStyle.firstCell;
		}

		@Override
		ObservableValue<String> getRowCellsStyle(TableStyle tableStyle) {
			return tableStyle.cell;
		}

		@Override
		ObservableValue<String> getRowLastCellsStyle(TableStyle tableStyle) {
			return tableStyle.lastCell;
		}

		// @Override
		// protected CellBuilder<String> getRowFirstCellBuilder() {
		// return new RowFirstCellTextCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<Table> getCellBuilder() {
		// return new TableCellBuilder<>();
		// }
		//
		// @Override
		// protected CellBuilder<String> getRowLastCellBuilder() {
		// return new RowLastCellButtonCellBuilder();
		// }
		//
		// @Override
		// protected CellBuilder<Table> getSecondCellBuilder() {
		// return new TableCellBuilder<>();
		// }

	}
}
