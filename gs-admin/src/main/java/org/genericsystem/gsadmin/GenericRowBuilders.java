package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericCellBuilders.RowLastCellButtonCellBuilder;
import org.genericsystem.gsadmin.GenericCellBuilders.TableCellBuilder;
import org.genericsystem.ui.table.CellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowLastCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.SecondCellBuilder;
import org.genericsystem.ui.table.CellBuilder.TextCellBuilder;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;

public abstract class GenericRowBuilders<COL, T> extends RowBuilder<COL, T> {

	@Override
	public Row build(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
			TableStyle tableStyle) {
		return new GenericRow((Generic) item, getFirstElement(firstColumnString, tableStyle), getSecondElement(secondColumnExtractor, tableStyle), getElements(columns, columnExtractor, tableStyle), getLastElement(lastColumnString, tableStyle),
				getStyle(tableStyle));
	}

	public static class TextCellRowBuilder<COL> extends GenericRowBuilders<COL, String> {

		@Override
		protected CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		protected CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder();
		}

		@Override
		protected CellBuilder<String> getRowLastCellBuilder() {
			return new RowLastCellButtonCellBuilder();
		}

		@Override
		protected CellBuilder<String> getSecondCellBuilder() {
			// TODO Auto-generated method stub
			return new SecondCellBuilder();
		}

	}

	public static class TextCellFirstRowBuilder<COL> extends TextCellRowBuilder<COL> {
		@Override
		protected CellBuilder<String> getRowFirstCellBuilder() {
			return new FirstRowFirstCellTextCellBuilder();
		}

		@Override
		protected CellBuilder<String> getCellBuilder() {
			return new FirstRowTextCellBuilder();
		}

		@Override
		protected CellBuilder<String> getRowLastCellBuilder() {
			return new FirstRowLastCellTextCellBuilder();
		}

		@Override
		protected CellBuilder<String> getSecondCellBuilder() {
			// TODO Auto-generated method stub
			return new SecondCellBuilder();
		}
	}

	public static final class TableCellRowBuilder<COL> extends GenericRowBuilders<COL, Table> {
		@Override
		protected CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		protected CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder<>();
		}

		@Override
		protected CellBuilder<String> getRowLastCellBuilder() {
			return new RowLastCellButtonCellBuilder();
		}

		@Override
		protected CellBuilder<Table> getSecondCellBuilder() {
			return new TableCellBuilder<>();
		}
	}
}
