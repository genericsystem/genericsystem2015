package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;

import org.genericsystem.ui.table.Stylable.TableStyle;

public abstract class CellBuilder<T> implements Builder {
	public Cell<T> build(ObservableValue<T> observableModel, TableStyle tableStyle) {
		return observableModel != null ? new Cell<T>(observableModel, getStyle(tableStyle)) : null;
	}

	public ObservableValue<String> getStyle(TableStyle tableStyle) {
		return tableStyle.cell;
	}

}
