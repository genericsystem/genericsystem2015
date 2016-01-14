package org.genericsystem.ui.table;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.ui.table.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	
	public Row(ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell, ObservableValue<String> styleClass) {
		super(firstCell, cells, lastCell, styleClass);
	}
}