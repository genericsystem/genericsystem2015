package org.genericsystem.gsadmin;

import java.util.function.Consumer;
import java.util.function.Function;

import javafx.beans.property.ObjectProperty;
import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import javafx.event.Event;
import javafx.event.EventHandler;
import javafx.scene.input.MouseDragEvent;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;

import org.genericsystem.gsadmin.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, T> implements Builder {

	protected Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return new Row(getFirstElement(firstColumnString, tableStyle), getElements(columns, columnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		if(firstColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	}

	protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), tableStyle));
	}
	
	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(getRowFirstCellBuilder()::init).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth)
				.setStyleClass(Cell<T>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(getCellBuilder()::init).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth).setStyleClass(Cell<T>::getStyleClass);
	}

	abstract CellBuilder<String> getRowFirstCellBuilder();

	abstract CellBuilder<T> getCellBuilder();

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

	static class TextCellRowBuilder<COL> extends RowBuilder<COL, String> {

		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder();
		}
	}

	static class TextCellFirstRowBuilder<COL> extends TextCellRowBuilder<COL> {
		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new FirstRowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<String> getCellBuilder() {
			return new FirstRowTextCellBuilder();
		}

		@Override
		public ObservableValue<String> getStyle(TableStyle tableStyle) {
			return tableStyle.firstRow;
		}
	}

	static final class TableCellRowBuilder<COL> extends RowBuilder<COL, Table> {

		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder<>();
		}
	}

}