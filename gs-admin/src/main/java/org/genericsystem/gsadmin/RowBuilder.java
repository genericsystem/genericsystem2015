package org.genericsystem.gsadmin;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.CellBuilder.TableCellBuilder;
import org.genericsystem.gsadmin.CellBuilder.TextCellBuilder;
import org.genericsystem.gsadmin.Stylable.TableStyle;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<T, COL> implements Builder {
	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(new TextCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(getCellBuilder()::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<T>::getStyleClass);
	}

	Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		if (firstColumnString == null)
			return null;
		ObservableValue<Cell<?>> firstCell = new ReadOnlyObjectWrapper<>(new TextCellBuilder().build(firstColumnString, getFirstCellStyle(tableStyle)));
		ObservableList<Cell<?>> cells = new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), getCellStyle(tableStyle)));
		return new Row(firstCell, cells, getRowStyle(tableStyle));
	}

	abstract CellBuilder<T> getCellBuilder();

	public ObservableValue<String> getRowStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

	ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	static class TextCellRowBuilder<COL> extends RowBuilder<String, COL> {
		@Override
		CellBuilder<String> getCellBuilder() {
			return new TextCellBuilder();
		}
	}

	static class TextCellFirstRowBuilder<COL> extends TextCellRowBuilder<COL> {
		@Override
		ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}

		@Override
		public ObservableValue<String> getRowStyle(TableStyle tableStyle) {
			return tableStyle.firstRow;
		}
	}

	static final class TableCellRowBuilder<COL> extends RowBuilder<Table, COL> {

		@Override
		CellBuilder<Table> getCellBuilder() {
			return new TableCellBuilder();
		}
	}

}