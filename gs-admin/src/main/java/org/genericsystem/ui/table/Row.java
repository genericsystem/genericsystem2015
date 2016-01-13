package org.genericsystem.ui.table;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	
	public Row(ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<String> styleClass) {
		super(firstCell, cells, styleClass);
	}
}