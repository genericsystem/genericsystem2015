package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.CellBuilder.FirstRowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.FirstRowTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.RowFirstCellTextCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, U, T> implements Builder {

	protected Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle) {
		return new Row(getFirstElement(firstColumnString, tableStyle), getElements(columns, columnExtractor, tableStyle), getStyle(tableStyle));
	}

	protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	}

	protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<String>> columnExtractor, TableStyle tableStyle) {
		return new Transformation<>(columns, column -> getCellBuilder().build((ObservableValue) columnExtractor.apply(column), tableStyle));
	}

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(getRowFirstCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(getCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
	}

	abstract CellBuilder<String> getRowFirstCellBuilder();

	abstract CellBuilder<T> getCellBuilder();

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

	static class TextCellRowBuilder<COL, U> extends RowBuilder<COL, U, String> {

		@Override
		CellBuilder<String> getRowFirstCellBuilder() {
			return new RowFirstCellTextCellBuilder();
		}

		@Override
		CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder();
		}
	}

	static class TextCellFirstRowBuilder<COL, U> extends TextCellRowBuilder<COL, U> {
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

	static final class TableCellRowBuilder<COL, U> extends RowBuilder<COL, U, Table> {

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