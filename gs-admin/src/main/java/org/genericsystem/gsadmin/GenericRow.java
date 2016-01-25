package org.genericsystem.gsadmin;

import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.beans.value.ObservableValue;
import javafx.collections.ObservableList;

import org.genericsystem.common.Generic;
import org.genericsystem.ui.table.Cell;
import org.genericsystem.ui.table.Row;
import org.genericsystem.ui.table.Table;

public class GenericRow extends Row {
	private StringProperty name = new SimpleStringProperty("");
	private Generic item;

	public GenericRow(Table parent, Generic item, ObservableValue<Cell<?>> secondCell, ObservableList<Cell<?>> cells, ObservableValue<Cell<?>> lastCell, ObservableValue<String> styleClass) {
		super(parent, secondCell, cells, lastCell, styleClass);
		this.item = item;
	}

	public GenericRow(Table parent) {
		super(parent, null, null, null, null);
	}

	public StringProperty getName() {
		return name;
	}

	public Generic getItem() {
		return item;
	}

	public void setItem(Generic item) {
		this.item = item;
	}

	public void add() {
		item.addInstance(name.get());
	}

	public void delete() {
		item.remove();
	}
}
