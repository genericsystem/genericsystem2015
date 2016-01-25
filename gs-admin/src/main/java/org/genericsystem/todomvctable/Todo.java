package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;
import org.genericsystem.ui.Model;

public class Todo extends Model {

	public Todo(TodoTableList parent) {
		super(parent);
	}

	Property<String> stringProperty = new SimpleStringProperty();

	public ObservableValue<String> getStringProperty() {
		return stringProperty;
	}
}