package org.genericsystem.ui.table;

import java.util.function.Function;

import javafx.beans.property.ReadOnlyObjectWrapper;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.Element;
import org.genericsystem.ui.components.GSHBox;
import org.genericsystem.ui.table.Stylable.TableStyle;
import org.genericsystem.ui.utils.Transformation;

public abstract class RowBuilder<COL, T> implements Builder {
	
	public abstract Row build(Object item,ObservableValue<String> firstColumnString, ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle);

	protected ObservableValue<Cell<?>> getFirstElement(ObservableValue<String> firstColumnString, TableStyle tableStyle) {
		if(firstColumnString.getValue() == null)
			return new ReadOnlyObjectWrapper<>();
		return new ReadOnlyObjectWrapper<>(getRowFirstCellBuilder().build(firstColumnString, tableStyle));
	}

	protected ObservableList<Cell<?>> getElements(ObservableList<COL> columns, Function<COL, ObservableValue<T>> columnExtractor, TableStyle tableStyle) {
		return new Transformation<>(columns, column -> getCellBuilder().build(columnExtractor.apply(column), tableStyle));
	}
	
	

	protected abstract CellBuilder<String> getRowFirstCellBuilder();

	protected abstract CellBuilder<T> getCellBuilder();

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.row;
	}

}