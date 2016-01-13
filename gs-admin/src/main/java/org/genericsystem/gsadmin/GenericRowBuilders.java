package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.CellBuilder;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.RowBuilder;
import org.genericsystem.ui.table.Table;
import org.genericsystem.ui.table.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.ui.table.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.GenericCellBuilders.*;
import org.genericsystem.ui.table.CellBuilder.TextCellBuilder;
import org.genericsystem.ui.table.Stylable.TableStyle;

public abstract class GenericRowBuilders<COL, T> extends RowBuilder<COL, T> {

	@Override
	public Row build(Object item, ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return new GenericRow((Generic) item, getFirstElement(firstColumnString, tableStyle), getElements(columns, columnExtractor, tableStyle), getStyle(tableStyle));
	}

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(GenericRow::getFirstElement).include(getRowFirstCellBuilder()::init).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		new GSHBox(rowPanel).forEach(GenericRow::getElements).include(getCellBuilder()::init).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);
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
		public void init(Element<?> parent) {
			new GSHBox(parent).select(GenericRow::getFirstElement).include(getRowFirstCellBuilder()::init).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth)
					.setStyleClass(Cell<Generic>::getStyleClass);
			new GSHBox(parent).forEach(GenericRow::getElements).include(getCellBuilder()::init).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<Generic>::getStyleClass);

		}
	}

}
