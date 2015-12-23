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

public interface RowBuilder<T, COL> {

	default Row build(ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		ObservableValue<Cell> firstCell = new ReadOnlyObjectWrapper<>(new CellBuilder() {
		}.build(firstColumnString, getFirstCellStyle(tableStyle)));
		ObservableList<Cell> cells = new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), getCellStyle(tableStyle)));
		return new Row(firstCell, cells, tableStyle.row);
	}

	public default void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(Row::getFirstElement).include(new TextCellBuilder() {
		}::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<Object>::getStyleClass);
		new GSHBox(rowPanel).forEach(Row::getElements).include(new TextCellBuilder() {
		}::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<Object>::getStyleClass);
	}

	default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
		return tableStyle.firstCell;
	}

	default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	default CellBuilder<T> getCellBuilder() {
		return new CellBuilder<T>() {
		};
	}

	static interface FirstRowBuilder<COL> extends RowBuilder<String, COL> {
		@Override
		default ObservableValue<String> getFirstCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowFirstCell;
		}

		@Override
		default ObservableValue<String> getCellStyle(TableStyle tableStyle) {
			return tableStyle.firstRowCell;
		}
	}

	static interface ExtendedRowBuilder<COL> extends RowBuilder<String, COL> {
		@Override
		public default void init(Element<?> rowPanel) {
			new GSHBox(rowPanel).select(Row::getFirstElement).include(new TextCellBuilder() {
			}::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<Object>::getStyleClass);
			new GSHBox(rowPanel).forEach(Row::getElements).include(new TableCellBuilder() {
			}::init).setPrefWidth(200).setMinHeight(80).setStyleClass(Cell<Object>::getStyleClass);
		}
	}

}