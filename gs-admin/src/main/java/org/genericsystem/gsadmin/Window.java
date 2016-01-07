package org.genericsystem.gsadmin;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleObjectProperty;
import javafx.beans.value.ObservableValue;

public class Window {

	private final ObservableValue<Number> width;
	private final ObservableValue<Number> height;

	private final Property<Table> table;
	private Property<Table> subTable=new SimpleObjectProperty<>();
	

	public Window(Property<Table> table, ObservableValue<? extends Number> width, ObservableValue<? extends Number> height) {
		this.table = table;
		this.width = (ObservableValue<Number>) width;
		this.height = (ObservableValue<Number>) height;
	}

	public Property<Table> getSubTable() {
		return subTable;
	}
	
	public ObservableValue<Number> getWidth() {
		return width;
	}

	public ObservableValue<Number> getHeight() {
		return height;
	}

	public Property<Table> getTable() {
		return table;
	}
	
	public void selectRow(Row row) {
		System.out.println(row.getItem());
	}
}
