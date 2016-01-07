package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;
import org.genericsystem.gsadmin.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	
	private Object item;
	
	public Row(Object item,ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<String> styleClass) {
		super(firstCell, cells, styleClass);
		this.item = item;
	}
	
	public <ITEM> ITEM getItem() {
		return (ITEM)item;
	}
}