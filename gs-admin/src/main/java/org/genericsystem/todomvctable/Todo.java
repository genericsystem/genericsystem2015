package org.genericsystem.todomvctable;

import javafx.beans.property.Property;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.value.ObservableValue;

public class Todo {

	Property<String> stringProperty = new SimpleStringProperty();

	public ObservableValue<String> getStringProperty() {
		return stringProperty;
	}
}