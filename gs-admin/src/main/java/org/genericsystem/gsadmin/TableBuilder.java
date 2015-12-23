package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Row.ExtendedRow;
import org.genericsystem.gsadmin.RowBuilder.TableRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.FirstRowBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.gsadmin.Table.ExtendedTable;
import org.genericsystem.ui.utils.Transformation;

public interface TableBuilder<ITEM, COL> {
	default Table build(TableModel<ITEM, COL> tableModel) {
		ObservableValue<Row> firstRow = new SimpleObjectProperty<>(new FirstRowBuilder() {
		}.build(tableModel.getRowfirstColumnString() != null ? tableModel.getFirstRowFirstColumnString() : null, tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle(), tableModel));
		ObservableList<Row> rows = new Transformation<Row, ITEM>(tableModel.getItems(), item -> new RowBuilder() {
		}.build(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null, tableModel.getColumns(), tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle(), getSubTableModel()));
		return build(firstRow, rows, tableModel.getTableStyle());
	}

	default Class<?> getElementClass() {
		return Row.class;
	}

	default TableModel getSubTableModel() {
		return null;
	}

	default Table build(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
		return new Table(firstRow, rows, tableStyle);
	}

	public static interface TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL> {

		@Override
		default Table build(TableModel<ITEM, COL> tableModel) {
			ObservableValue<Row> firstRow = new SimpleObjectProperty<>(new FirstRowBuilder() {
			}.build(tableModel.getRowfirstColumnString() != null ? tableModel.getFirstRowFirstColumnString() : null, tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle(), tableModel));
			ObservableList<Row> rows = new Transformation<Row, ITEM>(tableModel.getItems(), item -> new TableRowBuilder() {
			}.build(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null, tableModel.getColumns(), tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle(), getSubTableModel()));
			return build(firstRow, rows, tableModel.getTableStyle());
		}

		@Override
		default Class<?> getElementClass() {
			return ExtendedRow.class;
		}

		@Override
		default Table build(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
			return new ExtendedTable(firstRow, rows, tableStyle);
		}

		@Override
		default TableModel getSubTableModel() {
			return new TableModel<>(FXCollections.observableArrayList(8, 9), FXCollections.observableArrayList(7, 6));
		}
	}
}