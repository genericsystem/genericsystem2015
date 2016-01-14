package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericCellBuilders.RowLastCellButtonCellBuilder;
import org.genericsystem.gsadmin.GenericCellBuilders.TableCellBuilder;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.CellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowLastCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.TextCellBuilder;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.table.Table;

public abstract class GenericRowBuilders<COL, T> extends RowBuilder<COL, T> {

	@Override
	public Row build(Object item, ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString, TableStyle tableStyle) {
		return new GenericRow((Generic) item, getFirstElement(firstColumnString, tableStyle), getElements(columns, columnExtractor, tableStyle), getLastElement(lastColumnString, tableStyle), getStyle(tableStyle));
	}
	

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(GenericRow::getFirstElement).include(getRowFirstCellBuilder()::init).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		new GSHBox(rowPanel).forEach(GenericRow::getElements).include(getCellBuilder()::init).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
		new GSHBox(rowPanel).select(GenericRow::getLastElement).include(getRowLastCellBuilder()::init).setMinWidth(Table::getLastColumnWidth).setPrefWidth(Table::getLastColumnWidth).setMaxWidth(Table::getLastColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
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
	}
}
