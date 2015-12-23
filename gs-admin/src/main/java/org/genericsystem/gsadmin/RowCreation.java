package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Cell.ExtendedCell;
import org.genericsystem.gsadmin.Cell.FirstCell;
import org.genericsystem.gsadmin.Cell.FirstRowFirstCell;
import org.genericsystem.gsadmin.Cell.RowFirstCell;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public interface RowCreation<COL> {
	default Row create(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle, TableModel tableModel) {
		ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(Func.get(getFirstElementClass(), CellCreation.class).create(firstColumnString, getFirstCellStyle(tableStyle), null));
		ObservableList<Cell> cells = new Transformation<>(columns, column -> Func.get(getElementClass(), CellCreation.class).create(columnExtractor.apply(column), getCellStyle(tableStyle), tableModel));
		return new Row(firstCell, cells, tableStyle.row);
	};

	default Class<?> getFirstElementClass() {
		return RowFirstCell.class;
	}

	default Class<?> getElementClass() {
		return Cell.class;
	}

	default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	static interface FirstRowCreation<COL> extends RowCreation<COL> {
		@Override
		default Class<?> getFirstElementClass() {
			return FirstRowFirstCell.class;
		}

		@Override
		default Class<?> getElementClass() {
			return FirstCell.class;
		}

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
		default Class<?> getFirstElementClass() {
			return FirstRowFirstCell.class;
		}

		@Override
		default Class<?> getElementClass() {
			return ExtendedCell.class;
		}

	}

}