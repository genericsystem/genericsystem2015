package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.TextCell;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public interface RowCreation<COL> {
	default Row create(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle, TableModel tableModel) {
		ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(Func.get(getFirstElementClass(), CellCreation.class).create(firstColumnString, getFirstCellStyle(tableStyle)));
		ObservableList<Cell> cells = new Transformation<>(columns, column -> Func.get(getElementClass(), CellCreation.class).create(columnExtractor.apply(column), getCellStyle(tableStyle)));
		return new Row(firstCell, cells, tableStyle.row);
	};

	default Class<?> getFirstElementClass() {
		return TextCell.class;
	}

	default Class<?> getElementClass() {
		return TextCell.class;
	}

	default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	static interface FirstRowCreation<COL> extends RowCreation<COL> {

		@Override
		default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}

	}

	static interface ExtendedRowCreation<COL> extends RowCreation<COL> {

		@Override
		default Row create(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle, TableModel subTableModel) {
			ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(Func.get(getFirstElementClass(), CellCreation.class).create(firstColumnString, getFirstCellStyle(tableStyle)));
			ObservableValue observableTable = new ReadOnlyObjectWrapper<>(Func.get(Table.class, TableCreation.class).create(subTableModel));
			ObservableList<Cell> cells = new Transformation<>(columns, column -> Func.get(getElementClass(), CellCreation.class).create(observableTable, getCellStyle(tableStyle)));
			return new Row(firstCell, cells, tableStyle.row);
		};

		@Override
		default Class<?> getElementClass() {
			return ExtendedCell.class;
		}

	}

}