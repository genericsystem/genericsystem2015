package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.RowBuilder.FirstRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TextRowBuilder;
import org.genericsystem.gsadmin.RowBuilder.TableRowBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.gsadmin.Table.ExtendedTable;
import org.genericsystem.ui.utils.Transformation;

public interface TableBuilder<ITEM, COL> {
	default <T> Table build(TableModel<ITEM, COL, T> tableModel) {
		ObservableValue<Row> firstRow = new SimpleObjectProperty<>(new FirstRowBuilder() {
		}.build(tableModel.getRowfirstColumnString() != null ? tableModel.getFirstRowFirstColumnString() : null, tableModel.getColumns(), tableModel.getFirstRowExtractor(), tableModel.getTableStyle()));
		ObservableList<Row> rows = new Transformation<Row, ITEM>(tableModel.getItems(), item -> getRowBuilder().build(tableModel.getRowfirstColumnString() != null ? tableModel.getRowfirstColumnString().apply(item) : null, tableModel.getColumns(),
				tableModel.getRowColumnExtractor().apply(item), tableModel.getTableStyle()));
		return build(firstRow, rows, tableModel.getTableStyle());
	}

	default RowBuilder getRowBuilder() {
		return new TextRowBuilder() {
		};
	}

	default Table build(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
		return new Table(firstRow, rows, tableStyle);
	}

	public static interface TableCellTableBuilder<ITEM, COL> extends TableBuilder<ITEM, COL> {

		@Override
		default RowBuilder getRowBuilder() {
			return new TableRowBuilder() {
			};
		}

		@Override
		default Table build(ObservableValue<Row> firstRow, ObservableList<Row> rows, TableStyle tableStyle) {
			return new ExtendedTable(firstRow, rows, tableStyle);
		}
	}
}