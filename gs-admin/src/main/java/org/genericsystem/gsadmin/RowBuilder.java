package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public interface RowBuilder<T, COL> {

	default Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(new TextCellBuilder() {
		}.build(firstColumnString, getFirstCellStyle(tableStyle)));
		ObservableList<Cell> cells = new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), getCellStyle(tableStyle)));
		return new Row(firstCell, cells, tableStyle.row);
	}

	default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	CellBuilder<T> getCellBuilder();

	static interface TextRowBuilder<COL> extends RowBuilder<String, COL> {
		@Override
		default CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder() {
			};
		}
	}

	static interface FirstRowBuilder<COL> extends TextRowBuilder<COL> {

		@Override
		default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}
	}

	static interface TableRowBuilder<COL> extends RowBuilder<Table, COL> {
		@Override
		default CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder() {
			};
		}
	}

}