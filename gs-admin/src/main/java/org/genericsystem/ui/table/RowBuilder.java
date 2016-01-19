package org.genericsystem.ui.table;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, T> implements Builder {

	protected abstract Row build(Object item, ObservableValue<String> firstColumnString, ObservableValue<T> secondColumnExtractor, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, ObservableValue<String> lastColumnString,
			TableStyle tableStyle);

	protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		if (firstColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	}

	protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), tableStyle));
	}

	protected ObservableValue<Cell<?>> getLastElement(ObservableValue<String> lastColumnString, TableStyle tableStyle) {
		if (lastColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowLastCellBuilder().build(lastColumnString, tableStyle));
	}

	protected ObservableValue<Cell<?>> getSecondElement(ObservableValue<T> secondColumnString, TableStyle tableStyle) {
		return new ReadOnlyObjectWrapper<>(getSecondCellBuilder().build(secondColumnString, tableStyle));
	}

	protected abstract CellBuilder<String> getRowFirstCellBuilder();

	protected abstract CellBuilder<T> getCellBuilder();

	protected abstract CellBuilder<T> getSecondCellBuilder();

	protected abstract CellBuilder<String> getRowLastCellBuilder();

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

}