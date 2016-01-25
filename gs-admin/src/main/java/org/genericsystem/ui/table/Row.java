package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.table.Stylable.Listable;

public class Row extends Listable<Cell<?>> {

	public Row(Table parent, ObservableValue<Cell<?>> secondCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell, ObservableValue<String> styleClass) {
		super(parent, secondCell, cells, lastCell, styleClass);
	}

	// public Row(Table parent) {
	// super(parent, null, null, null, null);
	// parent
	// }
}