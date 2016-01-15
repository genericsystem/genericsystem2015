package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;

public class GenericRow extends Row{
	private StringProperty name = new SimpleStringProperty("");
	private Generic item;
	
	public GenericRow(Generic item, ObservableValue<Cell<?>> firstCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell, ObservableValue<String> styleClass) {
		super(firstCell, cells, lastCell, styleClass);
		this.item = item;
	}
	
	public StringProperty getName() {
		return name;
	}
	
	public Generic getItem() {
		return item;
	}
	
	public void add(){
		item.addInstance(name.get());
	}

	public void delete() {
		item.remove();
	}
}
