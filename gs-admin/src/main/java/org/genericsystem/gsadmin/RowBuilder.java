package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public interface RowBuilder<COL> {
	default Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle, TableModel tabledModel) {
		ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(new TextCellBuilder() {
		}.build(firstColumnString, getFirstCellStyle(tableStyle)));
		ObservableList<Cell> cells = new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), getCellStyle(tableStyle)));
		return new Row(firstCell, cells, tableStyle.row);
	}

	default CellBuilder getCellBuilder() {
		return new TextCellBuilder() {
		};
	}

	default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	static interface FirstRowBuilder<COL> extends RowBuilder<COL> {

		@Override
		default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}

	}

	static interface TableRowBuilder<COL> extends RowBuilder<COL> {

		@Override
		default Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle, TableModel subTableModel) {
			ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(new TextCellBuilder() {
			}.build(firstColumnString, getFirstCellStyle(tableStyle)));
			ObservableList<Cell> cells = new Transformation<>(columns, column -> getCellBuilder().build(new ReadOnlyObjectWrapper<>(new TableBuilder() {
			}.build(subTableModel)), getCellStyle(tableStyle)));
			return new Row(firstCell, cells, tableStyle.row);
		};

		@Override
		default CellBuilder getCellBuilder() {
			return new TableCellBuilder() {
			};
		}
	}

}