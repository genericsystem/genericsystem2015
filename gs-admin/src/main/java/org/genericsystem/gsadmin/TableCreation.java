package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Row.ExtendedRow;
import org.genericsystem.gsadmin.Row.FirstRow;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.gsadmin.Table.ExtendedTable;
import org.genericsystem.ui.utils.Transformation;

public interface TableCreation<ITEM, COL> {
	default Table create(TableModel<ITEM, COL> tableModel) {
		ObservableValue<Row> firstRow = new SimpleObjectProperty<>(Func.get(FirstRow.class, RowCreation.class).create(tableModel.getRowfirstColumnString() != null ? tableModel.getFirstRowFirstColumnString() : null, tableModel.getColumns(),
				tableModel.getFirstRowExtractor(), tableModel.getTableStyle(), tableModel));
		ObservableList<Row> rows = new Transformation<Row, ITEM>(tableModel.getItems(), item -> Func.get(getElementClass(), RowCreation.class).create(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null,
				tableModel.getColumns(), tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle(), getSubTableModel()));
		return createTable(firstRow, rows, tableModel.getTableStyle());
	}

	default Class<?> getElementClass() {
		return Row.class;
	}

	default TableModel getSubTableModel() {
		return null;
	}

	default Table createTable(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
		return new Table(firstRow, rows, tableStyle);
	}

	public static interface ExtendedTableCreation<ITEM, COL> extends TableCreation<ITEM, COL> {

		@Override
		default Class<?> getElementClass() {
			return ExtendedRow.class;
		}

		@Override
		default Table createTable(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
			return new ExtendedTable(firstRow, rows, tableStyle);
		}

		@Override
		default TableModel getSubTableModel() {
			return new TableModel<>(FXCollections.observableArrayList(8, 9), FXCollections.observableArrayList(7, 6));
		}
	}
}