package org.genericsystem.gsadmin.table.old;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.gsadmin.table.old.Stylable.Listable;

public class Row extends Listable<Cell<?>> {
	
	private Object item;
	private StringProperty name = new SimpleStringProperty("");
	
	public Row(Object item,ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<String> styleClass) {
		super(firstCell, cells, styleClass);
		this.item = item;
	}
	
	public <ITEM> ITEM getItem() {
		return (ITEM)item;
	}
	
	public StringProperty getName() {
		return name;
	}
	
	public void add(){
		((Generic)item).addInstance(name.get());
	}
}