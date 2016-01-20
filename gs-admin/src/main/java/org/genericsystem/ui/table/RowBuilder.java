package org.genericsystem.ui.table;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.GenericRow;
import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, T> implements Builder {

	@Override
	public void init(Element<?> rowPanel) {
		new GSHBox(rowPanel).select(GenericRow::getFirstElement).include(getRowFirstCellBuilder()::init).setMinWidth(Table::getFirstColumnWidth).setPrefWidth(Table::getFirstColumnWidth).setMaxWidth(Table::getFirstColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);

		new GSHBox(rowPanel).select(GenericRow::getSecondElement).include(getSecondCellBuilder()::init).setMinWidth(Table::getSecondColumnWidth).setPrefWidth(Table::getSecondColumnWidth).setMaxWidth(Table::getSecondColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);

		GSHBox elements = new GSHBox(rowPanel).forEach(GenericRow::getElements).include(getCellBuilder()::init).setMinWidth(Table::getColumnWidth).setPrefWidth(Table::getColumnWidth).setMaxWidth(Table::getColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
		{

		}
		new GSHBox(rowPanel).select(GenericRow::getLastElement).include(getRowLastCellBuilder()::init).setMinWidth(Table::getLastColumnWidth).setPrefWidth(Table::getLastColumnWidth).setMaxWidth(Table::getLastColumnWidth)
				.setStyleClass(Cell<Generic>::getStyleClass);
	}

	public abstract Row build(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
			TableStyle tableStyle);

	protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		if (firstColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	}

	protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		if (columnExtractor == null)
			return FXCollections.emptyObservableList();
		return new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), tableStyle));
	}

	protected ObservableValue<Cell<?>> getLastElement(ObservableValue<String> lastColumnString, TableStyle tableStyle) {
		if (lastColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowLastCellBuilder().build(lastColumnString, tableStyle));
	}

	protected ObservableValue<Cell<?>> getSecondElement(ObservableValue<T> secondColumnString, TableStyle tableStyle) {
		if (secondColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getSecondCellBuilder().build(secondColumnString, tableStyle));
	}

	// public Cell<T> buildCell(ObservableValue<T> observableModel, TableStyle tableStyle) {
	// return observableModel != null ? new Cell<T>(observableModel, getCellStyle(tableStyle)) : null;
	// }

	public ObservableValue<String> getCellStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

	protected abstract CellBuilder<String> getRowFirstCellBuilder();

	protected abstract CellBuilder<T> getCellBuilder();

	protected abstract CellBuilder<T> getSecondCellBuilder();

	protected abstract CellBuilder<String> getRowLastCellBuilder();

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

}