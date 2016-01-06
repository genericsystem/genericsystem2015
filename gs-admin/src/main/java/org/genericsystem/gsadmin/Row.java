package org.genericsystem.gsadmin;

import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	
	private Object item;
	
	public Row(Object item,ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<String> styleClass) {
		super(firstCell, cells, styleClass);
		this.item = item;
	}
	
	public void selected(){
		System.out.println(((Generic)this.item).getInstances().toList());
	}
}