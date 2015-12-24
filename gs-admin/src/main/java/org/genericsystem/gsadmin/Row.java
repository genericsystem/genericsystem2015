package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.gsadmin.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	public Row(ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<String> styleClass) {
		super(styleClass, firstCell, cells);
	}
}